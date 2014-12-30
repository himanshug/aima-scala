package aima.basic.simplerule

/** Framework for condition-action rules
 *
 * @author Himanshu Gupta
 */
class Rule[S,A](val condition: Condition[S], val action: A) {
  def execute(state: S):Boolean = condition.execute(state)
}

abstract class Condition[S] {
  def execute(state: S):Boolean
}

class EqualCondition[S](p: (S) => Boolean) extends Condition[S] {
  override def execute(state: S):Boolean = p(state) 
}

class NotCondition[S](cond:Condition[S]) extends Condition[S] {
  override def execute(state: S):Boolean = !cond.execute(state)
}

class AndCondition[S](left:Condition[S], right:Condition[S]) extends Condition[S] {
  override def execute(state: S):Boolean = left.execute(state) && right.execute(state)
}

class OrCondition[S](left:Condition[S], right:Condition[S]) extends Condition[S] {
  override def execute(state: S):Boolean = left.execute(state) || right.execute(state)
}
