package aima.search

/** A generic representation for Problem as described in section-3.1.1
 *
 * @author Himanshu Gupta
 */
abstract class Problem[S, A] {
  def initialState: S
  def goalTest(s: S): Boolean

  def actions(s: S): List[A]
  def result(s: S,a: A): S
 
  //provided one can go from TO to in single step, what is the
  //cost; c(s,a,s')
  def stepCost(from: S, action: A, to: S): Double

  //estimated cost to reach goal from
  //given state, heuristic function : h(n)
  def estimatedCostToGoal(from: S): Double
}

/** A generic representation for Non-Deterministic Problem as described
 * in section 4.3.1
 *
 * @author Himanshu Gupta
 */
abstract class NonDeterministicProblem[S,A](initState: S) extends Problem[S,A] {

  def initialState = initState

  def results(state: S, action: A): List[S]

  override def result(state: S, action: A):S =
    throw new UnsupportedOperationException("result does not exist for non-deterministic problem.")
}

/** A generic representation for Online-Search-Problem,
 * described in section 4.5.1
 *
 * @author Himanshu Gupta
 */
abstract class OnlineSearchProblem[P,A] extends Problem[P,A]{

  def initialState: P =
    throw new UnsupportedOperationException("initialState does not exist for Online problem.")

  def result(s: P,a: A): P =
    throw new UnsupportedOperationException("result does not exist for Online problem.")
}
