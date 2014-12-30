package aima.search.adversarial

/** Abstract representation of a Game,
 * described in section 5.1
 *
 * Type Params have following meanings
 * P -> Player
 * S -> Game State
 * A -> Action
 * 
 * @author Himanshu Gupta
 */
abstract class Game[P,S,A] {
  def initialState: S
  def player(s: S): P
  def actions(s: S): List[A]
  def result(s: S, a: A): S
  def terminalTest(s: S): Boolean
  def utility(s: S, p: P): Double
}

/** Abstract representation for 2 - Player games
 * named MIN and MAX traditionally, who play
 * against each other, MAX tries to bring the game
 * to a terminal state with maximum utility and MIN
 * does the opposite.
 *
 * described in section 5.1
 * 
 * @author Himanshu Gupta
 */
abstract class ZeroSumGame[S,A] extends Game[String,S,A] {

  //In two-player, zero-sum games, the two element vector
  //can be reduced to a single value because the values
  //are always opposite
  // -- described in Section 5.2.2
  override def utility(s: S, p: String) = utility(s)
  def utility(s: S): Double
}
object ZeroSumGame {
  val Min = "MIN"
  val Max = "MAX"
}
