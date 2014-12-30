package aima.search.informed

import junit.framework._
import Assert._

import aima.search._

class GreedyBestFirstSearchTest extends TestCase {

  def testRomaniaMap() {
    val p = new MapProblem(RomaniaMapFactory.createRomaniaMap(), In(RomaniaMapFactory.Arad), In(RomaniaMapFactory.Bucharest))
    GreedyBestFirstSearch(p) match {
      case Success(x) => assertEquals(x,List(Go(RomaniaMapFactory.Sibiu), Go(RomaniaMapFactory.Fagaras), Go(RomaniaMapFactory.Bucharest)))
      case CutOff() | Failure() => assertTrue(false)
    }
  }
}

// AStarSearch Test
class AStarSearchTest extends TestCase {

  def testRomaniaMap() {
    val p = new MapProblem(RomaniaMapFactory.createRomaniaMap(), In(RomaniaMapFactory.Arad), In(RomaniaMapFactory.Bucharest))
    AStarSearch(p) match {
      case Success(x) => assertEquals(x,List(Go(RomaniaMapFactory.Sibiu), Go(RomaniaMapFactory.RimnicuVilcea), Go(RomaniaMapFactory.Pitesti), Go(RomaniaMapFactory.Bucharest)))
      case CutOff() | Failure() => assertTrue(false)
    }
  }
}

// RecursiveBestFirstSearch Test
class RecursiveBestFirstSearchTest extends TestCase {

  def testRomaniaMap() {
    val p = new MapProblem(RomaniaMapFactory.createRomaniaMap(), In(RomaniaMapFactory.Arad), In(RomaniaMapFactory.Bucharest))
    RecursiveBestFirstSearch(p) match {
      case Success(x) => assertEquals(x,List(Go(RomaniaMapFactory.Sibiu), Go(RomaniaMapFactory.RimnicuVilcea), Go(RomaniaMapFactory.Pitesti), Go(RomaniaMapFactory.Bucharest)))
      case _ => assertTrue(false)
    }
  } 
}
