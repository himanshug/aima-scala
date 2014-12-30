package aima.search

/** Non-Deterministic Vacuum World Problem as described in
 * section 4.3.1
 *
 * state is represented by (agent-location,dirt-in-A?,dirt-in-B?)
 * possible actions are Suck/Left/Right
 *
 * @author Himanshu Gupta
 *
 */
class VacuumWorldNonDeterministicProblem(initLocation: String) extends NonDeterministicProblem[(String,Boolean,Boolean),String]((initLocation,true,true)) {
  private val Suck = "Suck"
  private val Right = "Right"
  private val Left = "Left"
  private val A = "A"
  private val B = "B"

  type State = (String,Boolean,Boolean)
  def actions(state: State): List[String] =
    state match {
      case (A,_,_) => List(Suck,Right)
      case (B,_,_) => List(Left,Suck)
      case _ => throw new IllegalStateException(state + " is not a valid vacuum world state")
    }

  def results(state: State, action: String):List[State] =
    (action,state) match {
      case (Left,(_,a,b)) => List((A,a,b))
      case (Right,(_,a,b)) => List((B,a,b))
      case (Suck, (A,true,true)) => List((A,false,false),(A,false,true))
      case (Suck, (A,true,false)) => List((A,false,false))
      case (Suck, (A,false,b)) => List((A,false,b),(A,true,b))
      case (Suck, (B,true,true)) => List((B,false,false),(B,true,false))
      case (Suck, (B,false,true)) => List((B,false,false))
      case (Suck, (B,a,false)) => List((B,a,true),(B,a,false))
      case _ => throw new IllegalStateException("Either invalid action: " + action + " or invalid state: " + state)
    }

  def goalTest(state: State) =
    state match {
      case (_,false,false) => true
      case _ => false
    }

  def stepCost(from: State,action: String, to :State): Double =
    throw new UnsupportedOperationException("stepCost is not supported.")

  def estimatedCostToGoal(state: State): Double =
    throw new UnsupportedOperationException("estimatedCostToGoal is not supported.")
}
