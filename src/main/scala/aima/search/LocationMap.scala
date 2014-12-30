package aima.search

/** A generic data structure to represent location maps such as
 * the map of Romania.
 *
 * @author Himanshu Gupta
 */
class LocationMap[A] {

  import scala.collection.mutable.Map

  private val map: Map[A,Map[A,Double]] = Map()

  //straight line distance between various pair of locations
  private val stDist : Map[(A,A),Double] = Map()
  
  def addLocation(location: A) {
    if(!map.contains(location))
      map += Tuple(location, Map[A,Double]())
  }
  
  def addPath(from: A, to: A, cost: Double, bidirectional: Boolean) {
    //add locations if they are not there already
    addLocation(from)
    addLocation(to)
    
    map.getOrElse(from,null) += Tuple(to,cost)
    if(bidirectional) map.getOrElse(to,null) += Tuple(from,cost)
  }
  
  //Add bi-directional path -- will be removed once scala-2.8 is in 
  //Scala-2.8 has concept of providing defaul value
  //to arguments, then we will not need this overloaded method
  //instead the above method signature will become
  //def addPath(from: Symbol, to: Symbol, cost: Double, bidirectional: Boolean = true)
  def addPath(from: A, to: A, cost: Double) { addPath(from,to,cost,true) }

  def addStraightLineDistance(loc1: A, loc2: A, dist: Double) {
    //add locations if not there already
    addLocation(loc1)
    addLocation(loc2)
    
    stDist.contains((loc1,loc2)) match {
      case true => stDist += Tuple((loc1,loc2),dist)
      case false => stDist += Tuple((loc2,loc1),dist)
    }
  }

  def getLocationsReachableFrom(from: A): List[A] =
    map.get(from) match {
      case Some(x) => x.keys.toList
      case None => throw new IllegalStateException(from + " is not in the map.")
    }

  //path distance from To to, provided they are adjacent
  def distance(from: A, to: A): Double = {
    map.get(from) match {
      case Some(tmp) => tmp.get(to) match {
                          case Some(x) => x
                          case None => throw new IllegalStateException(from + " and " + to + " are not adjacent."); }
      case None => throw new IllegalStateException(from + " is not a location.");
    }
  }

  //straight line distance between from AND to
  def straightLineDistance(from: A, to: A): Double =
    stDist.contains((from,to)) match {
      case true => stDist.getOrElse((from,to),0)
      case false if stDist.contains((to,from)) => stDist.getOrElse((to,from),0)
      case _ => throw new IllegalStateException("straight line distance between " + from + " and " + to + " is not available.")
    } 
}
