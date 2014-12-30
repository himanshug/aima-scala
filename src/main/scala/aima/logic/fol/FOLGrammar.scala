package aima.logic.fol

import scala.util.parsing.combinator._

// =========  Grammar for FOL logic =========


/** Parser for parsing FOL logic sentences
 *
 * Things to notice:
 *  ~ is used for negation
 *  & is used for conjunction(/\)
 *  | is used for disjunction(\/)
 *  =>,<=> are used for conditional and biconditional
 *  4L is used for "For All"
 *  3E is used for "There Exists"
 *
 *  Constant/Function/Predicate symbol are like [A-Z][a-zA-Z0-9]*
 *  Variable symbol is like [a-z][a-zA-Z0-9]*
 * 
 * @author Himanshu Gupta
 */
object FOLParser extends JavaTokenParsers {

  def sentence: Parser[Sentence] = (
    "4L"~repsep(variable,",")~biconditional ^^
    { case _~vs~s =>
      vs.map(Variable(_)).foldRight(s)(new UniversalQuantifier(_,_)) } //Universal Quantifier
    | "3E"~repsep(variable,",")~biconditional ^^
    { case _~vs~s =>
      vs.map(Variable(_)).foldRight(s)(new ExistentialQuantifier(_,_)) } //Existential Quantifier
    | biconditional
  )
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
  

  def negation: Parser[Sentence] = rep("~")~atomicSentence ^^
                                    {case Nil~as => as
                                     case list~as => list.foldLeft(as)((x,y)=> new Negation(x))}

  def atomicSentence: Parser[Sentence] = (
    term~"="~term ^^
    {case lt~"="~rt => new Equal(lt,rt)} //Equal
    | symbol~"("~repsep(term,",")~")" ^^
    {case s~"("~Nil~")" => new Predicate(s)
     case s~"("~ts~")" => new Predicate(s,ts:_*)} //Predicate
    | "(" ~> sentence <~ ")"
  )
                                         

  def term: Parser[Term] = (
    symbol~"("~repsep(term,",")~")" ^^
    {case s~"("~Nil~")" => new Function(s)
     case s~"("~ts~")" => new Function(s,ts:_*)}  //Function
    | symbol ^^ (Constant(_)) //Constant
    | variable ^^ (Variable(_)) //Variable
  )

  def variable = """[a-z][a-z0-9A-Z]*""".r //symbol for Variable
  def symbol = """[A-Z][a-z0-9A-Z]*""".r //symbol for Constant,Predicate,Function

  def parse(in: String) = parseAll(sentence,in).get

  //term parser, meant for Testing ONLY
  def parseTerm(in: String) = parseAll(term,in).get
}



//----------------------- FOL Sentence AST -------------------------------
abstract class Sentence

//--------- Term ---------
abstract class Term

//Constant
case class Constant(val symbol: String) extends Term {
  override def toString = symbol
}

//Function
class Function(val symbol:String, as: Term *) extends Term {
  val args = List(as:_*)

  override def equals(that: Any) =
    that match {
      case x: Function =>
        (x.symbol == this.symbol) && (x.args == this.args)
      case _ => false
    }

  override def toString =
    if(args == Nil)
      symbol + "()"
    else
      symbol + "(" + args.map(_.toString).reduceLeft(_ + "," + _) + ")"
}

//Variable
case class Variable(val symbol: String) extends Term {
  override def toString = symbol
}

//================= Atomic Sentence ============
abstract class AtomicSentence extends Sentence with FOLDefiniteClause

//Predicate
class Predicate(val symbol: String, as: Term *) extends AtomicSentence {
  val args = List(as:_*)

  override def equals(that: Any) =
    that match {
      case x: Predicate =>
        (x.symbol == this.symbol) && (x.args == this.args)
      case _ => false
    }

  override def toString =
    if(args == Nil)
      symbol + "()"
    else
      symbol + "(" + args.map(_.toString).reduceLeft(_ + "," + _) + ")"
}

//Equal
class Equal(val lTerm: Term, val rTerm: Term) extends AtomicSentence {
  override def equals(that: Any) =
    that match {
      case x: Equal => (x.lTerm == this.lTerm) && (x.rTerm == this.rTerm)
      case _ => false
    }

  override def toString = "(" + lTerm + " = " + rTerm + ")"
}

//=============== Complex Sentence ==============

//Negation
class Negation(val sentence: Sentence) extends Sentence {
  override def equals(that: Any) =
    that match {
      case x: Negation => x.sentence == this.sentence
      case _ => false
    }

  override def toString = "~" + sentence
}

//Conjunction
class Conjunction(cs: Sentence *) extends Sentence {
  val conjuncts: Set[Sentence] = Set(cs: _*)

  override def equals(that: Any) =
    that match {
      case x: Conjunction => x.conjuncts == this.conjuncts
      case _ => false
    }

  override def toString = "(" + conjuncts.map(_.toString).reduceLeft(_ + " /\\ " + _)  + ")"
}

//Disjunction
class Disjunction(ds: Sentence *) extends Sentence {
  val disjuncts: Set[Sentence] = Set(ds: _*)

  override def equals(that: Any) =
    that match {
      case x: Disjunction => x.disjuncts == this.disjuncts
      case _ => false
    }

  override def toString = "(" + disjuncts.map(_.toString).reduceLeft(_ + " \\/ " + _)  + ")"
}

//Conditional(or Implication)
class Conditional(val premise: Sentence, val conclusion: Sentence) extends Sentence {
  override def equals(that: Any) =
    that match {
      case x: Conditional => (x.premise == this.premise) && (x.conclusion == this.conclusion)
      case _ => false
    }

  override def toString = "(" + premise + " => " + conclusion + ")"
}

//BiConditional
class BiConditional(val condition: Sentence, val conclusion: Sentence) extends Sentence {
  override def equals(that: Any) =
    that match {
      case x: BiConditional => (x.condition == this.condition) && (x.conclusion == this.conclusion)
      case _ => false
    }

  override def toString = "(" + condition + " <=> " + conclusion + ")"
}

// Quantifiers
abstract class Quantifier(val variable: Variable, val sentence: Sentence) extends Sentence
class UniversalQuantifier(v: Variable,s: Sentence) extends Quantifier(v,s) {
  override def equals(that: Any) =
    that match {
      case x: UniversalQuantifier =>
        (x.variable == this.variable) && (x.sentence == this.sentence)
      case _ => false
    }

  override def toString = "(4L " + variable + " " + sentence + ")"
}
class ExistentialQuantifier(v: Variable, s: Sentence) extends Quantifier(v,s) {
  override def equals(that: Any) =
    that match {
      case x: ExistentialQuantifier =>
        (x.variable == this.variable) && (x.sentence == this.sentence)
      case _ => false
    }

  override def toString = "(3E " + variable + " " + sentence + ")"
}
