package aima.basic

import simplerule._

//P = Percept
//A = Action

/** Abstract Representation of Agent as described in Fig 2.1
 * 
 * @author Himanshu Gupta 
 */
abstract class Agent[P,A] {
  
  def agentProgram(percept: P): Option[A] //NoOp = None

  private var _isAlive: Boolean = true
  def isAlive = _isAlive
  def die() { _isAlive = false }

}

/** A trait for Table-Driven-Agent as described in Fig 2.7
 * 
 * @author Himanshu Gupta 
 */
trait TableDrivenAgent[P,A] extends Agent[P,A] {
  protected val table: Map[List[P],A]

  //percept sequence already seen
  private var percepts: List[P] = Nil
  
  override def agentProgram(percept: P) = {
    percepts = percepts ::: List(percept)
    table.get(percepts)
  }
}

/** A trait for Simple-Reflex-Agent as described in Fig 2.10
 * 
 * @author Himanshu Gupta 
 */
trait SimpleReflexAgent[S,P,A] extends Agent[P,A] with RuleProcessor[S,A]{

  protected def interpretInput(percept: P): S

  override def agentProgram(percept: P) = {
    val state = interpretInput(percept)
    val rule = ruleMatch(state,rules)
    ruleAction(rule)
  }
}

/** A trait for Model-Based-Reflex-Agent as described in Fig 2.12
 * 
 * @author Himanshu Gupta 
 */
trait ModelBasedReflexAgent[S,P,A] extends Agent[P,A] with RuleProcessor[S,A]{

  protected def updateState(state: S, action: Option[A], percept: P): S
  
  protected var state: S
  private var action: Option[A] = _ //most recent action

  override def agentProgram(percept: P) = {
    state = updateState(state,action,percept)
    val rule = ruleMatch(state, rules)
    action = ruleAction(rule)
    action
  }
}

/* A trait to provide re-usable simple condition-action rule processing */
trait RuleProcessor[S,A]{

  //a set of rules
  protected val rules: List[Rule[S,Option[A]]]

  protected def ruleMatch(state: S, rules: List[Rule[S,Option[A]]]) =
    rules.find(_.execute(state))

  protected def ruleAction(rule: Option[Rule[S,Option[A]]]): Option[A] =
    rule match {
      case None => None
      case Some(r: Rule[S,Option[A]]) => r.action
    }
}
