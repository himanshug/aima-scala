package aima.logic.propositional

import junit.framework._
import Assert._

/** Tests for Propositional Logic Grammar Parser
 *
 * @ Himanshu Gupta
 */
class PropositionalLogicParserTest extends TestCase {

  private val PLP = PropositionalLogicParser


  //some proposition symbols
  private val A = PropositionSymbol("A")
  private val B = PropositionSymbol("B")
  private val C = PropositionSymbol("C")
  private val D = PropositionSymbol("D")
  private val S = PropositionSymbol("S")
  private val T = PropositionSymbol("T")

  private val norvig = PropositionSymbol("norvig")
  private val aima = PropositionSymbol("aima")
  private val lisp = PropositionSymbol("lisp")
  private val cool = PropositionSymbol("cool")

  def testParser() {

    assertEquals(A,PLP.parse("A"))
    assertEquals(new Negation(A), PLP.parse("~ A"))
    assertEquals(new Negation(new Negation(A)), PLP.parse("~~A"))
    assertEquals(PropositionSymbol("True"),PLP.parse("True"))
    assertEquals(PropositionSymbol("False"),PLP.parse("False"))
    assertEquals(new Negation(PropositionSymbol("A12")),PLP.parse("~ A12"))
    assertEquals(new Disjunction(A,B),PLP.parse("A|B"))
    assertEquals(new Conjunction(A,B,C),PLP.parse("A & B & C"))
    assertEquals(new Disjunction(A,B,C),PLP.parse("A | B | C"))
    assertEquals(new Disjunction(A,new Conjunction(B,C)),PLP.parse("A | B & C"))
    assertEquals(new Disjunction(A,new Conjunction(new Negation(B),C)),PLP.parse("A | ~ B & C"))
    assertEquals(new Conditional(new Conjunction(A,B),C),PLP.parse("A & B => C"))
    assertEquals(new BiConditional(new Negation(A),new Conjunction(B,C)),PLP.parse("~A <=> (B & C)"))
    
    assertEquals(
      new Conditional(
        new Conjunction(new Negation(A),B),
        new Conjunction(S,T)),
      PLP.parse("~ A & B => S & T"))
    
    assertEquals(
      new Conditional(
        new Disjunction(new Negation(A),B),
        new Conjunction(S,T)),
      PLP.parse("~ A | B => S & T"))
    
    assertEquals(
      new Disjunction(new Negation(A),
                      new Conjunction(new Conditional(B,C),D)),
      PLP.parse("~ A | (B => C) & D"))
    
    assertEquals(
      new Conjunction(
        new Disjunction(norvig,aima,lisp),
        new Conditional(lisp,cool)),
    PLP.parse("( norvig | aima | lisp ) & (lisp => cool)"))
  }
}
