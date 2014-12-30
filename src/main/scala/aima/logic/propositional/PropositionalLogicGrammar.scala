package aima.logic.propositional

import scala.util.parsing.combinator._
import scala.collection.immutable.{Map,Set}

/** Parser for Propositional Logic Grammar
 * described in Fig 7.7
 *
 * Things to notice:
 * ~ is used for negation
 * & is used for conjunction(/\)
 * | is used for disjunction(\/)
 * =>,<=> are used for conditional,biconditional respectively
 * 
 * @author Himanshu Gupta
 */
object PropositionalLogicParser extends JavaTokenParsers {
  def sentence: Parser[Sentence] = biconditional

  def biconditional: Parser[Sentence] = conditional~opt("<=>"~conditional) ^^
                                      {case condition~None => condition
                                       case condition~Some(_~conclusion) => new BiConditional(condition,conclusion) }
  def conditional: Parser[Sentence] = disjunction~opt("=>"~disjunction) ^^
                                      {case premise~None => premise
                                       case premise~Some(_~conclusion) => new Conditional(premise,conclusion) }
  def disjunction: Parser[Sentence] = conjunction~rep("|"~conjunction) ^^
                                      {case c~Nil => c
                                       case c~reps => new Disjunction(c :: reps.map(_ match { case _~cond => cond }):_*)}
  def conjunction: Parser[Sentence] = negation~rep("&"~negation) ^^
                                      {case d~Nil => d
                                       case d~reps => new Conjunction(d :: reps.map(_ match { case _~disj => disj }):_*)}
  def negation: Parser[Sentence] = rep("~")~term ^^
                                    {case Nil~term => term
                                     case list~term => list.foldLeft(term)((x,y)=> new Negation(x))}
  def term: Parser[Sentence] = ident ^^ (PropositionSymbol(_)) | "("~>sentence<~")"

  def parse(in: String) = parseAll(sentence,in).get
}


// =========== AST for the propositional logic grammar ==============

abstract class Sentence {

  //Returns if this sentence is true/false in given model
  //None, if some symbol of the syntax is not defined in
  //the model
  def isTrue(model: Map[PropositionSymbol,Boolean]): Option[Boolean]

  //Returns the set of Symbols refered in this sentence
  def symbols: Set[PropositionSymbol]
}
object Sentence {

  //Knowledge Base = conjunction of sentences
  //Some convenience methods to deal with KB
  def createKB(s: Sentence *): Conjunction = new Conjunction(s:_*)

  def addToKB(KB: Conjunction, s: Sentence) =
    new Conjunction((KB.conjuncts + s).toList:_*)
} 

//Atomic Sentence
class PropositionSymbol private(val key: String) extends Sentence {

  def isTrue(model: Map[PropositionSymbol,Boolean]) = {
    if(key == PropositionSymbol.True) Some(true)
    else if(key == PropositionSymbol.False) Some(false)
    else model.get(this) 
  }

  def symbols = 
    key match {
      case PropositionSymbol.True => Set[PropositionSymbol]()
      case PropositionSymbol.False => Set[PropositionSymbol]()
      case _ => Set(this)
    }

  override def equals(that: Any) =
    that match {
      case x : PropositionSymbol => x.key == this.key
      case _ => false
    }

  override def toString = key

  override def hashCode = key.hashCode
}  
object PropositionSymbol {

  val True = "True"
  val False = "False"

  private val cache = scala.collection.mutable.Map[String,PropositionSymbol](
    True -> new PropositionSymbol(True),
    False -> new PropositionSymbol(False))
  
  //Note: This is not ideal in a multi-threaded
  //executional environment, more than one instance,
  //for same key might be created, BUT this should
  //cause no error
  def apply(name: String) =
    cache.get(name) match {
      case None =>
        val s = new PropositionSymbol(name)
        cache += (name -> s)
        s
      case Some(s) => s
    }
}

//Complex Sentences
class Negation(val s: Sentence) extends Sentence {
  def isTrue(model: Map[PropositionSymbol,Boolean]) = 
    s.isTrue(model) match {
      case Some(x) => Some(!x)
      case None => None
    }

  def symbols = s.symbols

  override def equals(that: Any) =
    that match {
      case x: Negation => x.s == s
      case _ => false
    }

  override def toString = "~" + s
}

class Conjunction(cs: Sentence *) extends Sentence {

  val conjuncts: Set[Sentence] = Set(cs: _*)
 
  def isTrue(model: Map[PropositionSymbol,Boolean]) = {
    //even if a single one is false, it is false
    if(conjuncts.exists(_.isTrue(model) == Some(false)))
      Some(false)
    else {
      if(conjuncts.exists(_.isTrue(model) == None))
        None
      else Some(true)
    }
  }

  def symbols = conjuncts.flatMap(_.symbols)

  override def equals(that: Any) =
    that match {
      case x: Conjunction => x.conjuncts == conjuncts
      case _ => false
    }

  override def toString = "(" + conjuncts.map(_.toString).reduceLeft(_ + " /\\ " + _)  + ")"
}

class Disjunction(ds: Sentence *) extends Sentence {

  val disjuncts: Set[Sentence] = Set(ds: _*)

  def isTrue(model: Map[PropositionSymbol,Boolean]) = {
    //even if a single one is true, it is true
    if(disjuncts.exists(_.isTrue(model) == Some(true)))
      Some(true)
    else {
      if(disjuncts.exists(_.isTrue(model) == None))
        None
      else Some(false)
    }
  }

  def symbols = disjuncts.flatMap(_.symbols)

  override def equals(that: Any) =
    that match {
      case x: Disjunction => x.disjuncts == disjuncts
      case _ => false
    }

  override def toString = "(" + disjuncts.map(_.toString).reduceLeft(_ + " \\/ " + _)  + ")"
}

class Conditional(val premise: Sentence, val conclusion: Sentence) extends Sentence {

  def isTrue(model: Map[PropositionSymbol,Boolean]) =
    (premise.isTrue(model),conclusion.isTrue(model)) match {
      case (Some(true),Some(true)) => Some(true)
      case (Some(true),Some(false)) => Some(false)
      case (Some(false),Some(_)) => Some(true)
      case (_,None) => None
      case (None,_) => None
    }

  def symbols = premise.symbols ++ conclusion.symbols

  override def equals(that: Any) =
    that match {
      case x: Conditional => (x.premise == premise) && (x.conclusion == conclusion)
      case _ => false
    }

  override def toString = "(" + premise + " => " + conclusion + ")"
}

class BiConditional(val condition: Sentence, val conclusion: Sentence) extends Sentence {

  def isTrue(model: Map[PropositionSymbol,Boolean]) =
    (condition.isTrue(model),conclusion.isTrue(model)) match {
      case (Some(true),Some(true)) => Some(true)
      case (Some(true),Some(false)) => Some(false)
      case (Some(false),Some(false)) => Some(true)
      case (Some(false),Some(true)) => Some(false)
      case (_,None) => None
      case (None,_) => None
    }

  def symbols = condition.symbols ++ conclusion.symbols

  override def equals(that: Any) =
    that match {
      case x: BiConditional => (x.condition == condition) && (x.conclusion == conclusion)
      case _ => false
    }

  override def toString = "(" + condition + " <=> " + conclusion + ")"
}
