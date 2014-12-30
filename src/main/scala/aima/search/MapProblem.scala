package aima.search

case class In[A](val location: A) //"State"
case class Go[A](val location: A) //"Action"

/** Generic MapProblem to describe route finding problems.
 *
 * @author Himanshu Gupta
 */
class MapProblem(locationMap: LocationMap[Symbol], initState: In[Symbol], goalState: In[Symbol]) extends Problem[In[Symbol],Go[Symbol]] {

  override def initialState = initState

  override def goalTest(s: In[Symbol]): Boolean = s == goalState

  override def actions(s: In[Symbol]): List[Go[Symbol]] =
    s match {
      case In(x) => locationMap.getLocationsReachableFrom(x).map(Go(_))
    }

  override def result(s: In[Symbol], a: Go[Symbol]): In[Symbol] = {
    val Go(x) = a
    In(x)
  }

  override def stepCost(from: In[Symbol], action: Go[Symbol], to: In[Symbol]): Double =
    (from,to) match {
      case (In(f),In(t)) => locationMap.distance(f,t) }

  override def estimatedCostToGoal(from: In[Symbol]): Double =
    (from,goalState) match {
      case (In(f),In(g)) => locationMap.straightLineDistance(f,g)}
}
