package aima.logic.propositional

import junit.framework._
import Assert._

/* This file contains tests for CNF sentence Grammar
 * described in Fig 7.14
 *
 * @author Himanshu Gupta
 */

class CNFGrammarTest extends TestCase {
  
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

  def testIt() {
    assertEquals(new CNFSentence(Set(new Clause(PositiveLiteral(A)))),SentenceToCNF(PLP.parse("A")))
    assertEquals(new CNFSentence(Set(new Clause(NegativeLiteral(A)))),SentenceToCNF(PLP.parse("~ A")))

    assertEquals(new CNFSentence(Set(new Clause(
      PositiveLiteral(A),
      PositiveLiteral(B)))),SentenceToCNF(PLP.parse("A | B")))

    assertEquals(new CNFSentence(Set(
      new Clause(PositiveLiteral(A)),
      new Clause(PositiveLiteral(B)),
      new Clause(PositiveLiteral(C)))),
      SentenceToCNF(PLP.parse("A & B & C")))

    assertEquals(new CNFSentence(Set(
      new Clause(
        PositiveLiteral(A),
        PositiveLiteral(B),
        PositiveLiteral(C)))),
        SentenceToCNF(PLP.parse("A | B | C")))

    assertEquals(new CNFSentence(Set(
      new Clause(PositiveLiteral(A),
                 PositiveLiteral(B)),
      new Clause(PositiveLiteral(A),
                 PositiveLiteral(C)))),
                 SentenceToCNF(PLP.parse("A | B & C")))

    assertEquals(new CNFSentence(Set(
      new Clause(PositiveLiteral(A),
                 NegativeLiteral(B)),
      new Clause(PositiveLiteral(A),
                 PositiveLiteral(C)))),
                 SentenceToCNF(PLP.parse("A | ~ B & C")))
    
    assertEquals(new CNFSentence(Set(
      new Clause(
        NegativeLiteral(A),
        NegativeLiteral(B),
        PositiveLiteral(C)))),
                 SentenceToCNF(PLP.parse("A & B => C")))

    assertEquals(new CNFSentence(Set(
      new Clause(PositiveLiteral(A),
                 PositiveLiteral(B)),
      new Clause(PositiveLiteral(A),
                 PositiveLiteral(C)),
      new Clause(
        NegativeLiteral(A),
        NegativeLiteral(B),
        NegativeLiteral(C)))),
                 SentenceToCNF(PLP.parse("~A <=> (B & C)")))

    assertEquals(new CNFSentence(Set(
      new Clause(
        PositiveLiteral(A),
        NegativeLiteral(B),
        PositiveLiteral(S)),
      new Clause(
        PositiveLiteral(A),
        NegativeLiteral(B),
        PositiveLiteral(T)))),
                 SentenceToCNF(PLP.parse("~A & B => S & T")))

    assertEquals(new CNFSentence(Set(
      new Clause(PositiveLiteral(A),
                 PositiveLiteral(S)),
      new Clause(PositiveLiteral(A),
                 PositiveLiteral(T)),
      new Clause(NegativeLiteral(B),
                 PositiveLiteral(S)),
      new Clause(NegativeLiteral(B),
                 PositiveLiteral(T)))),
                 SentenceToCNF(PLP.parse("~ A | B => S & T")))

    assertEquals(new CNFSentence(Set(
      new Clause(
        NegativeLiteral(A),
        NegativeLiteral(B),
        PositiveLiteral(C)),
      new Clause(
        NegativeLiteral(A),
        PositiveLiteral(D)))),
                 SentenceToCNF(PLP.parse("~ A | (B => C) & D")))

    assertEquals(new CNFSentence(Set(
      new Clause(
        PositiveLiteral(norvig),
        PositiveLiteral(aima),
        PositiveLiteral(lisp)),
      new Clause(
        NegativeLiteral(lisp),
        PositiveLiteral(cool)))),
                 SentenceToCNF(PLP.parse("( norvig | aima | lisp ) & (lisp => cool)")))
  }
}
