package aima.planning.classical

import scala.util.parsing.combinator._

/** Propositional Planning Problems representation(ones with no variables),
 * that Planning Graphs work with, described in section 10.3, 3rd paragraph
 *
 * To, instantiate, one should use ClassicalPlanningProblem(String,String,Action*)
 * Strings format is two type only:
 * 1. "Have(Cake)" OR "~Have(Cake)" for +ve/-ve literals respectively
 * 2. "Have(Cake) & ~Eaten(Cake)": notice '&' is used for conjunction
 * All constants are alphanumerics only and have to start with a capital letter(A-Z)
 * 
 * @author Himanshu Gupta
 */
class ClassicalPlanningProblem private(val initState: Set[Literal], val goals: Set[Literal],
                               val actions: Set[Action])
object ClassicalPlanningProblem {
  def apply(initState: String, goals: String, actions: Action *) =
    new ClassicalPlanningProblem(
      if(!initState.trim.isEmpty) CPSentenceParser.parseSet(initState) else Set.empty,
      if(!goals.trim.isEmpty) CPSentenceParser.parseSet(goals) else Set.empty,
      Set(actions:_*))
}


//Action -
//Notice, two Actions are equal as long as their symbols are equal, even if they
//have different preconditions and/or effects. That is "overloading" is not
//supported
class Action private(val symbol: Atom, val preconditions: Set[Literal], val effects: Set[Literal]) {

  override def equals(that: Any) =
    that match {
      case x: Action => this.symbol == x.symbol
      case _ => false
    }

  override def hashCode = symbol.hashCode
  
  override def toString = symbol.toString
}
object Action {
  def apply(symbol: String, preconds: String, effects: String) =
    new Action(
      CPSentenceParser.parseAtom(symbol),
      if(!preconds.trim.isEmpty) CPSentenceParser.parseSet(preconds) else Set.empty,
      if(!effects.trim.isEmpty) CPSentenceParser.parseSet(effects) else Set.empty)

  def noOp(l: Literal) =
    new Action(new Atom("$NoOp:" + l.toString + "$",Nil),
               Set(l),Set(l))
}

//---------- AST for Classical Planning Variable Free Sentence ---------
abstract class Sentence

//Used to represent a predicate( e.g. Have(Cake))
//as well as an action name(e.g. Eat(Cake))
class Atom(val symbol: String, val args: List[String]) extends Sentence {

  override def equals(that: Any) =
    that match {
      case x: Atom =>
        (symbol == x.symbol) && (args == x.args)
      case _ => false
    }

  override def hashCode = toString.hashCode

  override def toString = {
    symbol +
    (if(args.size == 0) "()" else "(" + args.reduceLeft(_ + "," + _) + ")")
  }
}

class Negation(val sentence: Atom) extends Sentence
class Conjunction(val conjuncts: Set[Sentence]) extends Sentence

//Positive and Negative Literals
sealed class Literal(val sentence: Atom) {

  def isPositive =
    (this: @unchecked) match { //case _:Literal is missing, so @unchecked
      case _: PositiveLiteral => true
      case _: NegativeLiteral => false
    }

  def isNegative = !isPositive

  override def toString =
    (if(isNegative) "~" else "") + sentence.toString
}
case class PositiveLiteral(s: Atom) extends Literal(s)
case class NegativeLiteral(s: Atom) extends Literal(s)


//---------------- Parser ---------------------
object CPSentenceParser extends JavaTokenParsers {


  def sentence: Parser[Sentence] = negation~rep("&"~negation) ^^
                                      {case d~Nil => d
                                       case d~reps => new Conjunction(Set((d :: reps.map(_ match { case _~disj => disj })):_*))}
  

  def negation: Parser[Sentence] = opt("~")~atom ^^
                                    {case None~as => as
                                     case Some(_)~as => new Negation(as)}

  def atom: Parser[Atom] = (
    constant~opt("("~repsep(constant,",")~")") ^^
    {case c~None => new Atom(c,Nil)
     case c~Some("("~ts~")") => new Atom(c,ts)}
    //constant~"("~repsep(constant,",")~")" ^^
    //{case s~"("~ts~")" => new Atom(s,ts)}
  )
                                         
  def constant = """[A-Z][a-z0-9A-Z]*""".r

  def parseSet(in: String): Set[Literal] =
    parseAll(sentence,in).get match {
      case x: Atom => Set(PositiveLiteral(x))
      case x: Negation => Set(NegativeLiteral(x.sentence))
      case x: Conjunction =>
        x.conjuncts.map(
          _ match {
            case y: Atom => PositiveLiteral(y)
            case y: Negation => NegativeLiteral(y.sentence)
          })
    }

  def parseAtom(in: String): Atom =
    parseAll(atom,in).get
}
