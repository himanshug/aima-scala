package aima.search.uninformed

import junit.framework._
import Assert._

import aima.search._

class BreadthFirstTreeSearchTest extends TestCase {

  def test8QueensProblem() { //successful search test
    BreadthFirstTreeSearch(new NQueensProblem(8)) match {
      case Success(x) => assertEquals(x,List(Put(8), Put(4), Put(1), Put(3), Put(6), Put(2), Put(7), Put(5)))
      case _ => assertTrue(false) 
    }
  }

  def test3QueensProblem() { //unsuccessful search test
    BreadthFirstTreeSearch(new NQueensProblem(3)) match {
      case Failure() => assertTrue(true)
      case _ => assertTrue(false)
    }
  }
}

class BreadthFirstGraphSearchTest extends TestCase {

  def test8QueensProblem() { //successful search test
    BreadthFirstGraphSearch(new NQueensProblem(8)) match {
      case Success(x) => assertEquals(x,List(Put(8), Put(4), Put(1), Put(3), Put(6), Put(2), Put(7), Put(5)))
      case _ => assertTrue(false) 
    }
  }

  def test3QueensProblem() { //unsuccessful search test
    BreadthFirstGraphSearch(new NQueensProblem(3)) match {
      case Failure() => assertTrue(true)
      case _ => assertTrue(false)
    }
  }

  def testRomaniaMapAradToBucharest() {
    val p = new MapProblem(RomaniaMapFactory.createRomaniaMap(), In(RomaniaMapFactory.Arad), In(RomaniaMapFactory.Bucharest))
//    Success(List(Go('Sibiu), Go('Fagaras), Go('Bucharest)))
    BreadthFirstGraphSearch(p) match {
      case Success(x) => assertEquals(x,List(Go('Sibiu), Go('Fagaras), Go('Bucharest)))
      case _ => assertTrue(false)
    }
  }
}

class DepthFirstTreeSearchTest extends TestCase {

  def test8QueensProblem() { //successful search test
    DepthFirstTreeSearch(new NQueensProblem(8)) match {
      case Success(x) => assertEquals(x,List(Put(1), Put(5), Put(8), Put(6), Put(3), Put(7), Put(2), Put(4)))
      case _ => assertTrue(false) 
    }
  }

  def test3QueensProblem() { //unsuccessful search test
    DepthFirstTreeSearch(new NQueensProblem(3)) match {
      case Failure() => assertTrue(true)
      case _ => assertTrue(false)
    }
  }
}

class DepthFirstGraphSearchTest extends TestCase {

  def test8QueensProblem() { //successful search test
    DepthFirstGraphSearch(new NQueensProblem(8)) match {
      case Success(x) => assertEquals(x,List(Put(1), Put(5), Put(8), Put(6), Put(3), Put(7), Put(2), Put(4)))
      case _ => assertTrue(false) 
    }
  }

  def test3QueensProblem() { //unsuccessful search test
    DepthFirstGraphSearch(new NQueensProblem(3)) match {
      case Failure() => assertTrue(true)
      case _ => assertTrue(false)
    }
  }

  def testRomaniaMapAradToBucharest() {
    val p = new MapProblem(RomaniaMapFactory.createRomaniaMap(), In(RomaniaMapFactory.Arad), In(RomaniaMapFactory.Bucharest))
    DepthFirstGraphSearch(p) match {
      case Success(x) => assertEquals(x.last,Go('Bucharest)) //it'll find different path in different execution
      case _  => assertTrue(false)
    }
  }
}

class UniformCostSearchTest extends TestCase {

  def testRomaniaMapAradToBucharest() {
    val p = new MapProblem(RomaniaMapFactory.createRomaniaMap(), In(RomaniaMapFactory.Arad), In(RomaniaMapFactory.Bucharest))
    //Success(List(Go('Sibiu), Go('Fagaras), Go('Bucharest)))
    UniformCostSearch(p) match {
      case Success(x) => assertEquals(x,List(Go('Sibiu), Go('Rimnicu_Vilcea), Go('Pitesti), Go('Bucharest)))
      case _  => assertTrue(false)
    }
  }

  //test described in Fig 3.15
  def testRomaniaMapSibiuToBucharest() {
    val p = new MapProblem(RomaniaMapFactory.createRomaniaMap(), In(RomaniaMapFactory.Sibiu), In(RomaniaMapFactory.Bucharest))
    UniformCostSearch(p) match {
      case Success(x) => assertEquals(x,List(Go('Rimnicu_Vilcea), Go('Pitesti), Go('Bucharest)))
      case _  => assertTrue(false)
    }
  }
}

class DepthLimitedSearchTest extends TestCase {

  def testSuccessful() {
    DepthLimitedSearch(new NQueensProblem(8),8) match {
      case Success(x) => assertEquals(x,List(Put(8), Put(4), Put(1), Put(3), Put(6), Put(2), Put(7), Put(5)))
      case _ => assertTrue(false)
    }
  }

  def testCutoff() {
    DepthLimitedSearch(new NQueensProblem(8),7) match {
      case CutOff() => assertTrue(true)
      case _ => assertTrue(false)
    }
  }

  def testFailure() {
    DepthLimitedSearch(new NQueensProblem(3),5) match {
      case Failure() => assertTrue(true)
      case _ => assertTrue(false)
    }
  }
}

class IterativeDeepeningSearchTest extends TestCase {

  def testIt() {
    IterativeDeepeningSearch(new NQueensProblem(8)) match {
      case Success(x) => assert(x == List(Put(8), Put(4), Put(1), Put(3), Put(6), Put(2), Put(7), Put(5)))
      case _ => assertTrue(false)
    }
  }
}
