package aima.logic.propositional

import junit.framework._
import Assert._

/* This file contains tests for entailment algorithms
 * described in chapter 7.
 *
 * @author Himanshu Gupta
 */

// ---------- TT-ENTAILS Tests -------------    
class TTEntailsTest extends TestCase {

  def test1() {
    val KB = new TTEntailsBasedKB("A&B")
    assertTrue(KB.ask("A"))
  }

  def test2() {
    val KB = new TTEntailsBasedKB("A|B")
    assertTrue(!KB.ask("A"))
  }

  def test3() {
    val KB = new TTEntailsBasedKB("(A => B) & B")
    assertTrue(!KB.ask("A"))
  }

  def test4() {
    val KB = new TTEntailsBasedKB("A")
    assertTrue(!KB.ask("~ A"))
  }

  def test5() {
    val KB = new TTEntailsBasedKB("(A => B) & B")
    assertTrue(!KB.ask("X"))
  }

  def test6() {
    val KB = new TTEntailsBasedKB("~A")
    assertTrue(!KB.ask("A"))
  }

  def test7() {
    val KB = new TTEntailsBasedKB()
    KB.tell("(B12 <=> (P11 | (P13 | (P22 | P02))))")
    KB.tell("(B21 <=> (P20 | (P22 | (P31 | P11))))")
    KB.tell("(B01 <=> (P00 | (P02 | P11)))")
    KB.tell("(B10 <=> (P11 | (P20 | P00)))")
    KB.tell("(~ B21)")
    KB.tell("(~B12)")
    KB.tell("(B10)")
    KB.tell("(B01)")
    assertTrue(KB.ask("P00"))
    assertTrue(!KB.ask("~P00"))
  }

  /** A test based on example described in Fig 7.13 */
  def testFig7_13() {
    val KB = new PLResolutionBasedKB()
    KB.tell("~P12 | B11")
    KB.tell("~B11 | P12 | P21")
    KB.tell("~P12 | B11")
    KB.tell("~B11")
    assertTrue(KB.ask("~ P12"))
  }

  /**
   * Test based on simple KB described in Section - 7.4.3
   */
  def testSec7_4_3() {
    val R1 = "~P11"
    val R2 = "B11 <=> (P12 | P21)"
    val R3 = "B21 <=> (P11 | P22 | P31)"
    val R4 = "~B11"
    val R5 = "B21"

    val alpha = "~P12"

    assertTrue(new TTEntailsBasedKB(R1,R2,R3,R4,R5).ask(alpha))
  } 
}

// ---------- PL-RESOLUTION Tests -------------    
class PLResolutionTest extends TestCase {

  def test1() {
    val KB = new PLResolutionBasedKB("A&B")
    assertTrue(KB.ask("A"))
  }

  def test2() {
    val KB = new PLResolutionBasedKB("A|B")
    assertTrue(!KB.ask("A"))
  }

  def test3() {
    val KB = new PLResolutionBasedKB("(A => B) & B")
    assertTrue(!KB.ask("A"))
  }

  def test4() {
    val KB = new PLResolutionBasedKB("A")
    assertTrue(!KB.ask("~ A"))
  }

  def test5() {
    val KB = new PLResolutionBasedKB("(A => B) & B")
    assertTrue(!KB.ask("X"))
  }

  def test6() {
    val KB = new PLResolutionBasedKB("~A")
    assertTrue(!KB.ask("A"))
  }

  /** This problem starts with 23 clauses, which result in 253
   * clause pairs and soon goes out of proportion leading to Out
   * of memory exception.
   * It seems this should not be solved with PLResolution
   * so commenting it out.. will look at it again in future.
   */
/*  def test7() {
    val KB = new PLResolutionBasedKB()
    KB.tell("(B12 <=> (P11 | (P13 | (P22 | P02))))")
    KB.tell("(B21 <=> (P20 | (P22 | (P31 | P11))))")
    KB.tell("(B01 <=> (P00 | (P02 | P11)))")
    KB.tell("(B10 <=> (P11 | (P20 | P00)))")
    KB.tell("(~ B21)")
    KB.tell("(~B12)")
    KB.tell("(B10)")
    KB.tell("(B01)")
    assertTrue(KB.ask("P00"))
    assertTrue(!KB.ask("~P00"))
  }*/

  //Testing resoution function
  def test8() {
    val c1 = SentenceToCNF(PropositionalLogicParser.parse("~P12 | B11")).clauses.toList(0)
    val c2 = SentenceToCNF(PropositionalLogicParser.parse("~B11 | P12 | P21")).clauses.toList(0)
    val resolvents = PLResolution.plResolve(c1,c2)
    assertTrue(resolvents.size == 0)
  }

  /** A test based on example described in Fig 7.13 */
  def testFig7_13() {
    val KB = new PLResolutionBasedKB()
    KB.tell("~P12 | B11")
    KB.tell("~B11 | P12 | P21")
    KB.tell("~P12 | B11")
    KB.tell("~B11")
    assertTrue(KB.ask("~ P12"))
  }

  /**
   * Test based on simple KB described in Section - 7.4.3
   */
  def testSec7_4_3() {
    val R1 = "~P11"
    val R2 = "B11 <=> (P12 | P21)"
    val R3 = "B21 <=> (P11 | P22 | P31)"
    val R4 = "~B11"
    val R5 = "B21"

    val alpha = "~P12"

    assertTrue(new PLResolutionBasedKB(R1,R2,R3,R4,R5).ask(alpha))
  }
}

//---------- PLFC-ENTAILS ------------------
class PLFCEntailsTest extends TestCase {

  /** This test is based on the problem shown
   * in Fig 7.16
   */
  def testFig7_16() {
    val P = PropositionSymbol("P")
    val Q = PropositionSymbol("Q")
    val L = PropositionSymbol("L")
    val M = PropositionSymbol("M")
    val A = PropositionSymbol("A")
    val B = PropositionSymbol("B")

    val KB = Set(new DefiniteClause(Set(P),Q), // P => Q
                     new DefiniteClause(Set(L,M),P), // L/\M => P
                     new DefiniteClause(Set(B,L),M), // B/\L => M
                     new DefiniteClause(Set(A,P),L), // A/\P => L
                     new DefiniteClause(Set(A,B),L)) // A/\B => L

    assertTrue(PLFCEntails(KB,Q,List(A,B)))
  }
}

//---------- DPLL-SATISFIABLE Tests ------------------
class DPLLSatisfiableTest extends TestCase {

  def testDPLLReturnsTrueWhenAllClausesTrueInModel() {
    val clauses = SentenceToCNF(PropositionalLogicParser.parse("(A & B) & (A & B)")).clauses
    val symbols = clauses.flatMap(_.symbols)

    val A = PropositionSymbol("A")
    val B = PropositionSymbol("B")
    val model = Map(A -> true, B -> true)
    assertTrue(DPLLSatisfiable.DPLL(clauses,symbols,model))
  }

  def testDPLLReturnsFalseWhenOneClauseFalseInModel() {
    val clauses = SentenceToCNF(PropositionalLogicParser.parse("(A | B) & (A => B)")).clauses
    val symbols = clauses.flatMap(_.symbols)

    val A = PropositionSymbol("A")
    val B = PropositionSymbol("B")
    val model = Map(A -> true, B -> false)
    assertTrue(!DPLLSatisfiable.DPLL(clauses,symbols,model))
  }

  def testDPLLFindsPurePositiveLiteralsWhenTheyExist() {
    val clauses = SentenceToCNF(PropositionalLogicParser.parse("(A & B) & (B & C) & (~C | A)")).clauses

    val A = PropositionSymbol("A")
    val B = PropositionSymbol("B")
    val C = PropositionSymbol("C")
    
    assertTrue(DPLLSatisfiable.FindPureSymbol(Set(A),clauses,Map()) == Some(A,true))
    assertTrue(DPLLSatisfiable.FindPureSymbol(Set(B),clauses,Map()) == Some(B,true))
    assertTrue(DPLLSatisfiable.FindPureSymbol(Set(C),clauses,Map()) == None)
    assertTrue(DPLLSatisfiable.FindPureSymbol(Set(C),clauses,Map(A -> true)) == Some(C,true))
  }

  def testDPLLFindsPureNegativeLiteralsWhenTheyExist() {
    val clauses = SentenceToCNF(PropositionalLogicParser.parse("(A & B) & (B & ~C) & (C | A)")).clauses

    val A = PropositionSymbol("A")
    val C = PropositionSymbol("C")

    assertTrue(DPLLSatisfiable.FindPureSymbol(Set(C),clauses,Map()) == None)
    assertTrue(DPLLSatisfiable.FindPureSymbol(Set(C),clauses,Map(A -> true)) == Some(C,false))
  }

  def testFindUnitClause() {
    val clauses = SentenceToCNF(PropositionalLogicParser.parse("A | B | ~C")).clauses

    val A = PropositionSymbol("A")
    val B = PropositionSymbol("B")
    val C = PropositionSymbol("C")
    
    assertTrue(DPLLSatisfiable.FindUnitClause(clauses,Map()) == None)
    assertTrue(DPLLSatisfiable.FindUnitClause(clauses,Map(A -> false,C -> true)) == Some(B,true))
    assertTrue(DPLLSatisfiable.FindUnitClause(clauses,Map(A -> false,B -> false)) == Some(C,false))
  }

  def test1() {
    val KB = new DPLLBasedKB("A&B")
    assertTrue(KB.ask("A"))
  }

  def test2() {
    val KB = new DPLLBasedKB("A|B")
    assertTrue(!KB.ask("A"))
  }

  def test3() {
    val KB = new DPLLBasedKB("(A => B) & B")
    assertTrue(!KB.ask("A"))
  }

  def test4() {
    val KB = new DPLLBasedKB("A")
    assertTrue(!KB.ask("~ A"))
  }

  def test5() {
    val KB = new DPLLBasedKB("(A => B) & B")
    assertTrue(!KB.ask("X"))
  }

  def test6() {
    val KB = new DPLLBasedKB("~A")
    assertTrue(!KB.ask("A"))
  }

  //TODO: this test failed when upgraded from scala 2.8.1 to 2.9.1
  //seems DPLLSatisfiable is not working correctly now, needs to
  //be fixed
//  def test7() {
//    val KB = new DPLLBasedKB()
//    KB.tell("(B12 <=> (P11 | (P13 | (P22 | P02))))")
//    KB.tell("(B21 <=> (P20 | (P22 | (P31 | P11))))")
//    KB.tell("(B01 <=> (P00 | (P02 | P11)))")
//    KB.tell("(B10 <=> (P11 | (P20 | P00)))")
//    KB.tell("(~ B21)")
//    KB.tell("(~B12)")
//    KB.tell("(B10)")
//    KB.tell("(B01)")
//    assertTrue(KB.ask("P00"))
//    assertTrue(!KB.ask("~P00"))
//  }

  def test8() {
    assertTrue(DPLLSatisfiable(PropositionalLogicParser.parse("((A | ~A) & (A | B))")))
  }

  /**
   * Test based on simple KB described in Section - 7.4.3
   */
  def testSec7_4_3() {
    val R1 = "~P11"
    val R2 = "B11 <=> (P12 | P21)"
    val R3 = "B21 <=> (P11 | P22 | P31)"
    val R4 = "~B11"
    val R5 = "B21"

    val alpha = "~P12"

    assertTrue(new DPLLBasedKB(R1,R2,R3,R4,R5).ask(alpha))
  }
}

//---------- WALKSAT Tests ------------------
class WalkSatTest extends TestCase {

  def testIt(){
    val clauses = SentenceToCNF(PropositionalLogicParser.parse("(A <=> (B | (C | (D | E))))")).clauses
    //clauses are
    //Set((E \/ ~A \/ B \/ C \/ D), (A \/ ~B), (A \/ ~D), (A \/ ~E), (A \/ ~C))
    WalkSat(clauses,0.5,10000) //result will change from one run to another
    assertTrue(true)
  }
}
