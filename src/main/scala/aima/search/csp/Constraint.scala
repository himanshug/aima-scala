package aima.search.csp

/** Absract Representation for a *binary* Constraint
 *
 * @author Himanshu Gupta
 */
abstract class Constraint[K,V](x1: K, x2: K) {
  
  def variables = (x1,x2)

  //Returns whether given assignment is complete w.r.t
  //this constraint i.e. does given assignment assigns
  //values to x1 and x2 both
  def isComplete(assignment: Map[K,V]) =
    (assignment.get(x1),assignment.get(x2)) match {
      case (Some(_),Some(_)) => true
      case _ => false
    }

  //Returns whether given assignment is complete w.r.t
  //this constraint and values assigned are in agreement
  //with this constraint
  def isSatisfied(assignment: Map[K,V]): Boolean
}

/**A generic All-Diff Constraint, puts the constraint that
 * values to both variables should be different
 *
 * @author Himanshu Gupta
 */
class AllDiffConstraint[K,V](x1: K, x2: K) extends Constraint[K,V](x1,x2) {
  def isSatisfied(assignment: Map[K,V]) =
    (assignment.get(x1),assignment.get(x2)) match {
      case (Some(val1),Some(val2)) => val1 != val2
      case _ => false
    }
}
object AllDiffConstraint {

  //A convenience method to create (n combination 2) AllDiffConstraint
  //for given list of n variables
  def createConstraints[K,V](xs: List[K]): List[AllDiffConstraint[K,V]] = {
    
    def loop(xs: List[K], result: List[AllDiffConstraint[K,V]]): List[AllDiffConstraint[K,V]] =
      xs match {
        case x :: rest =>
          loop(rest, result ++ rest.map(new AllDiffConstraint[K,V](x,_)))
        case Nil => result
      }

    loop(xs,Nil)
  }
}
