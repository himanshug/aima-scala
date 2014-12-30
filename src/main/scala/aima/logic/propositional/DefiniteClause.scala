package aima.logic.propositional

/** A type for DefiniteClauseForm described in
 * Fig 7.14
 *
 * @author Himanshu Gupta
 */
class DefiniteClause(val premise: Set[PropositionSymbol], val conclusion: PropositionSymbol) {
  override def toString =
    "(" + premise.map(_.toString).reduceLeft(_ + " /\\ " + _) + ") => " + conclusion.toString
}
