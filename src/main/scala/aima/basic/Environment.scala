package aima.basic

/** Abstract Environment representation.
 *
 * Type parameter P is the Percept and A is the Action.
 * 
 * @author Himanshu Gupta
 */
abstract class Environment[T <: Agent[P,A],P,A] {
  private var agents: List[T] = Nil
  private var views: List[(Option[A])=>Unit] = Nil

  def executeAction(agent: T, action: Option[A]): Unit
  def getPerceptSeenBy(agent: T): P

  def registerView(view: (Option[A])=>Unit) { views = view :: views }

  def updateViews(action: Option[A]) { views.foreach(view => view(action)) }

  def addAgent(agent: T) { agents = agent :: agents }

  def isDone() = !agents.exists(a => a.isAlive)

  def step() {
    if(!this.isDone) {
      agents.foreach(
        agent => {
          val anAction = agent.agentProgram(getPerceptSeenBy(agent))
          updateViews(anAction)
          this.executeAction(agent, anAction) })
    }
  }

  def step(n:Int) {
    if(n>0) {
      step()
      step(n - 1)
    }
  }

  def stepUntilNoOp() {
    if(!this.isDone()) {
      step()
      stepUntilNoOp()
    }
  }
}
