package aima.logic.propositional

import scala.collection.immutable.{Set}

/** AST for the CNF grammar described in Fig 7.14
 *
 * @author Himanshu Gupta
 */
class CNFSentence(val clauses: Set[Clause]) {
  
  def isTrue(model: Map[PropositionSymbol,Boolean]) = {
    //even if a single one is false, it is false
    if(clauses.exists(_.isTrue(model) == Some(false)))
      Some(false)
    else {
      if(clauses.exists(_.isTrue(model) == None))
        None
      else Some(true)
    }
  }

  override def equals(that: Any) =
    that match {
      case x: CNFSentence => x.clauses == clauses
      case _ => false
    }

  override def toString =
    clauses.size match {
      case 0 => "()"
      case _ =>
        "(" + clauses.map(_.toString).reduceLeft(_ + " /\\ " + _) + ")"
    }
}

class Clause(ls: Literal *) {
  val literals: Set[Literal] = Set(ls: _*)

  def isEmpty = literals.isEmpty

  def isTrue(model: Map[PropositionSymbol,Boolean]) = {
    //even if a single one is true, it is true
    if(literals.exists(_.isTrue(model) == Some(true)))
      Some(true)
    else {
      if(literals.exists(_.isTrue(model) == None))
        None
      else Some(false)
    }
  }

  def symbols: Set[PropositionSymbol] = literals.flatMap(_.symbols)

  override def equals(that: Any) =
    that match {
      case x: Clause => x.literals == this.literals
      case _ => false
    }

  override def hashCode = literals.hashCode
    
  override def toString =
    literals.size match {
      case 0 => "()"
      case _ =>
        "(" + literals.map(_.toString).reduceLeft(_ + " \\/ " + _) + ")" 
    }
}

abstract class Literal(val symbol: PropositionSymbol) {
  def isTrue(model: Map[PropositionSymbol,Boolean]): Option[Boolean]
  def symbols: Set[PropositionSymbol] = symbol.symbols
}

case class PositiveLiteral(s: PropositionSymbol) extends Literal(s) {
  def isTrue(model: Map[PropositionSymbol,Boolean]) =
    symbol.isTrue(model)

  override def toString = symbol.toString
}
case class NegativeLiteral(s: PropositionSymbol) extends Literal(s) {
  def isTrue(model: Map[PropositionSymbol,Boolean]) =
    symbol.isTrue(model) match {
      case Some(x) => Some(!x)
      case None => None
    }

  override def toString = "~" + symbol.toString
}

// Propositional Logic sentence to CNF sentence converter based on
//INDO method described in
//http://logic.stanford.edu/classes/cs157/2009/lectures/lecture04.pdf
object SentenceToCNF {
  def apply(s: Sentence) : CNFSentence =
    new CNFSentence(convert(s,false))

  def convert(s: Sentence, isNegated: Boolean): Set[Clause] = {
        //Implications out, eliminate all the occurences of
    //=> and <=> ops
    var result = removeImplications(s)

    //Negations In, negations are distributed over other
    //logical operators untill each negation applies only to
    //single Atomic Sentence
    result = negationsIn(result)

    //Operators Out,distribute \/ and /\ so that sentence becomes
    //conjunction of disjunctions and then return Set[Clause]
    removeOperators(result)
  }

  def removeImplications(s: Sentence): Sentence =
    s match {
      case x: PropositionSymbol => x
      case x: Negation =>
        new Negation(removeImplications(x.s))
      case x: Conjunction =>
        new Conjunction(x.conjuncts.map(removeImplications(_)).toList:_*)
      case x: Disjunction =>
        new Disjunction(x.disjuncts.map(removeImplications(_)).toList:_*)
      case x: Conditional =>
        new Disjunction(new Negation(x.premise),x.conclusion)
      case x: BiConditional =>
        new Conjunction(
          new Disjunction(new Negation(x.condition),x.conclusion),
          new Disjunction(x.condition,new Negation(x.conclusion)))
    }

  def negationsIn(s: Sentence) = {

    def negationsIn(s:Sentence, shouldNegate: Boolean): Sentence =
      if(shouldNegate) negationsIn(negate(s),false)
      else {
        s match {
          case x: PropositionSymbol => x
          case x: Negation =>
            x.s match {
              case _: PropositionSymbol => x
              case _ => negationsIn(x.s,true)
            }
          case x: Conjunction =>
            new Conjunction(x.conjuncts.map(negationsIn(_,false)).toList:_*)
          case x: Disjunction =>
            new Disjunction(x.disjuncts.map(negationsIn(_,false)).toList:_*)
        }
      }
    negationsIn(s,false)
  }

  //Remove Operators
  def removeOperators(s: Sentence): Set[Clause] =
    s match {
      case x: PropositionSymbol => Set(new Clause(PositiveLiteral(x)))
      case x: Negation if x.s.isInstanceOf[PropositionSymbol] =>
        Set(new Clause(NegativeLiteral(x.s.asInstanceOf[PropositionSymbol])))
      case x: Conjunction =>
        x.conjuncts.flatMap(removeOperators(_))
      case x: Disjunction =>
        x.disjuncts.map(removeOperators(_)).reduceLeft(unionOfTwoClauseSets(_,_))
    }

  //Returns negation of a sentence
  def negate(s: Sentence): Sentence =
    s match {
      case x: PropositionSymbol => new Negation(x)
      case x: Negation => x.s
      case x: Conjunction =>
        new Disjunction(x.conjuncts.map(new Negation(_)).toList:_*)
      case x: Disjunction =>
        new Conjunction(x.disjuncts.map(new Negation(_)).toList:_*)
      case x: Conditional =>
        new Conjunction(x.premise,new Negation(x.conclusion))
      case x: BiConditional =>
        new Disjunction(
          new Conjunction(x.condition, new Negation(x.conclusion)),
          new Conjunction(new Negation(x.condition),x.conclusion))
    }

  /** Union of two sets of clauses:
   * (C11 /\ C12 /\ C13.... /\ C1N) \/ (C21 /\ C22 /\ C23..../\ C2M) =
   * 
   * (((C11 /\ C12 /\ C13.... /\ C1N) \/ C21) /\
   *  ((C11 /\ C12 /\ C13.... /\ C1N) \/ C22) /\
   *  ((C11 /\ C12 /\ C13.... /\ C1N) \/ C23) /\
   * ...
   * ...
   *  ((C11 /\ C12 /\ C13.... /\ C1N) \/ C2M)) =
   *
   * (((C11 \/ C21) /\ (C12 \/ C21) /\ (C13 \/ C21).../\ (C1N \/ C21)) /\
   *  ((C11 \/ C22) /\ (C12 \/ C22) /\ (C13 \/ C22).../\ (C1N \/ C22)) /\
   *  ((C11 \/ C23) /\ (C12 \/ C23) /\ (C13 \/ C23).../\ (C1N \/ C23)) /\
   *  ...
   *  ...
   *  ((C11 \/ C2M) /\ (C12 \/ C2M) /\ (C13 \/ C2M).../\ (C1N \/ C2M)))
   *
   * Union of two clauses  = a new clause with all the literals there in
   * the two clauses
   *
   * so effectively the result is the set of all clauses that come by combining
   * each clause from the first set with each clause in the second set
   *
   * total # of resulting clauses = N X M
   */
  def unionOfTwoClauseSets(cs1: Set[Clause], cs2: Set[Clause]): Set[Clause] =
    for(ci <- cs1; cj <- cs2) yield new Clause(List((ci.literals ++ cj.literals).toList:_*).toList:_*)
}
