package aima.logic.fol

import junit.framework._
import Assert._

/** Tests for SentenceToCNF Converter Test
 *
 * @author Himanshu Gupta
 */
class SentenceToCNFTest extends TestCase {

  //These tests are based on the "loves animal KB" described in Section 9.5.3
  def testLovesAnimalA() {

    val clause1 = new Clause(
      PositiveLiteral(new Predicate("Loves",new Function("F$$1", Variable("x$$3")),
                                    Variable("x$$3"))),
      NegativeLiteral(new Predicate("Loves",Variable("x$$3"),
                                    new Function("F$$2", Variable("x$$3")))))

    val clause2 = new Clause(
      PositiveLiteral(new Predicate("Animal",new Function("F$$2", Variable("x$$2")))),
      PositiveLiteral(new Predicate("Loves",new Function("F$$1", Variable("x$$2")),Variable("x$$2"))))


    assertEquals(Set(clause1,clause2),
                 SentenceToCNF(FOLParser.parse("4L x (4L y Animal(y) => Loves(x,y)) => (3E y Loves(y,x))"),new FOLKnowledgeBase))
  }

  def testLovesAnimalB() {
    val clause = new Clause(
      NegativeLiteral(
        new Predicate("Animal",Variable("z$$1"))),
      NegativeLiteral(
        new Predicate("Kills",Variable("x$$2"),Variable("z$$1"))),
      NegativeLiteral(
        new Predicate("Loves",Variable("y$$3"),Variable("x$$2"))))

    val clausesB = new FOLKnowledgeBase().tell("4L x (3E z Animal(z) & Kills(x,z)) => (4L y ~Loves(y,x))").clauses
    assertEquals(Set(clause),
                 SentenceToCNF(FOLParser.parse("4L x (3E z Animal(z) & Kills(x,z)) => (4L y ~Loves(y,x))"),new FOLKnowledgeBase))
  }

  def testLovesAnimalC() {
    val clause = new Clause(
      NegativeLiteral(new Predicate("Animal",Variable("x$$1"))),
      PositiveLiteral(new Predicate("Loves",Constant("Jack"),Variable("x$$1"))))

    assertEquals(Set(clause),
                 SentenceToCNF(FOLParser.parse("4L x Animal(x) => Loves(Jack,x)"),new FOLKnowledgeBase))
  }

  def testLovesAnimalD() {
    val clause = new Clause(
      PositiveLiteral(new Predicate("Kills", Constant("Jack"),Constant("Tuna"))),
      PositiveLiteral(new Predicate("Kills", Constant("Curiosity"),Constant("Tuna"))))

    assertEquals(Set(clause),
                 SentenceToCNF(FOLParser.parse("Kills(Jack,Tuna) | Kills(Curiosity,Tuna)"),new FOLKnowledgeBase))
  }

  def testLovesAnimalE() {
    val clause = new Clause(
      PositiveLiteral(new Predicate("Cat", Constant("Tuna"))))
    assertEquals(Set(clause),
                 SentenceToCNF(FOLParser.parse("Cat(Tuna)"),new FOLKnowledgeBase))
  }

  def testLovesAnimalF() {
    val clause = new Clause(
      NegativeLiteral(new Predicate("Cat",Variable("x$$1"))),
      PositiveLiteral(new Predicate("Animal",Variable("x$$1"))))

    assertEquals(Set(clause),
                 SentenceToCNF(FOLParser.parse("4L x Cat(x) => Animal(x)"),new FOLKnowledgeBase))
  }
}
