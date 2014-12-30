package aima.search.local

import junit.framework._
import Assert._

import aima.search.{Success,VacuumWorldNonDeterministicProblem}

//AndOrSearch Tests
class AndOrGraphSearchTest extends TestCase {
  def testIt() {
    val problem = new VacuumWorldNonDeterministicProblem("A")
    AndOrGraphSearch(problem) match {
      case Success(x) => assertEquals(x.toString(),"Suck IF (A,false,false) THEN [NoOp] ELSE  IF (A,false,true) THEN [Right Suck NoOp]")
      case _ => assertTrue(false)
    }
  }
}
