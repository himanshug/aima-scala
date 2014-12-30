package aima.logic.fol

import junit.framework._
import Assert._

/** Tests for FOL-FC-ASK
 *
 * @author Himanshu Gupta
 */
class FOLFCAskTest extends TestCase {

  def testWeaponsKBCriminalWestSuccess() {
    val query = FOLParser.parse("Criminal(West)").asInstanceOf[AtomicSentence]
    assertEquals(1,FOLFCAsk(KBFactory.weaponsKB,query).size)
  }

  def testWeaponsKBCriminalXReturnsWest() {
    val query = FOLParser.parse("Criminal(x)").asInstanceOf[AtomicSentence]
    val result = FOLFCAsk(KBFactory.weaponsKB,query)
    assertEquals(1,result.size)
    
    val expectedPair = Variable("x") -> Constant("West")
    assertTrue(result.exists( _.exists(_ == expectedPair)))
  }

  def testKingsKBRichardEvilFalse() {
    val query = FOLParser.parse("Evil(Richard)").asInstanceOf[AtomicSentence]
    assertTrue(FOLFCAsk(KBFactory.kingsKB,query).isEmpty)
  }

  def testKingsKBJohnEvilSuccess() {
    val query = FOLParser.parse("Evil(John)").asInstanceOf[AtomicSentence]
    assertEquals(1,FOLFCAsk(KBFactory.kingsKB,query).size)
  }

  def testKingsKBEvilXReturnsJohn() {
    val query = FOLParser.parse("Evil(x)").asInstanceOf[AtomicSentence]

    val result = FOLFCAsk(KBFactory.kingsKB,query)
    assertEquals(1,result.size)

    val expectedPair = Variable("x")->Constant("John")
    assertTrue(result.exists(_.exists(_ == expectedPair)))
  }

  def testKingsKBKingXReturnsJohnAndRichard() {
    val query = FOLParser.parse("King(x)").asInstanceOf[AtomicSentence]

    val result = FOLFCAsk(KBFactory.kingsKB,query)
    assertEquals(2,result.size)

    val ep1 = Variable("x")->Constant("John")
    val ep2 = Variable("x")->Constant("Richard")

    assertTrue(result.exists(_.exists(_ == ep1)))
    assertTrue(result.exists(_.exists(_ == ep2)))
  }
}


/** Tests for FOL-BC-ASK
 *
 * @author Himanshu Gupta
 */
class FOLBCAskTest extends TestCase {

  def testWeaponsKBCriminalWestSuccess() {
    val query = FOLParser.parse("Criminal(West)").asInstanceOf[AtomicSentence]
    assertEquals(1,FOLBCAsk(KBFactory.weaponsKB,query).size)
  }

  def testWeaponsKBCriminalXReturnsWest() {
    val query = FOLParser.parse("Criminal(x)").asInstanceOf[AtomicSentence]
    val result = FOLBCAsk(KBFactory.weaponsKB,query)
    assertEquals(1,result.size)
    
    val expectedPair = Variable("x") -> Constant("West")
    assertTrue(result.exists( _.exists(_ == expectedPair)))
  }

  def testKingsKBRichardEvilFalse() {
    val query = FOLParser.parse("Evil(Richard)").asInstanceOf[AtomicSentence]
    assertTrue(FOLBCAsk(KBFactory.kingsKB,query).isEmpty)
  }

  def testKingsKBJohnEvilSuccess() {
    val query = FOLParser.parse("Evil(John)").asInstanceOf[AtomicSentence]
    assertEquals(1,FOLBCAsk(KBFactory.kingsKB,query).size)
  }

  def testKingsKBEvilXReturnsJohn() {
    val query = FOLParser.parse("Evil(x)").asInstanceOf[AtomicSentence]

    val result = FOLBCAsk(KBFactory.kingsKB,query)
    assertEquals(1,result.size)

    val expectedPair = Variable("x")->Constant("John")
    assertTrue(result.exists(_.exists(_ == expectedPair)))
  }

  def testKingsKBKingXReturnsJohnAndRichard() {
    val query = FOLParser.parse("King(x)").asInstanceOf[AtomicSentence]

    val result = FOLBCAsk(KBFactory.kingsKB,query)
    assertEquals(2,result.size)

    val ep1 = Variable("x")->Constant("John")
    val ep2 = Variable("x")->Constant("Richard")

    assertTrue(result.exists(_.exists(_ == ep1)))
    assertTrue(result.exists(_.exists(_ == ep2)))
  }
}

/** Tests for FOL Resolution
 *
 * @author Himanshu Gupta
 */
class FOLResolutionTest extends TestCase {

  def testCuriosityKillsTunaSucceeds() {
    assertTrue(FOLResolution(KBFactory.lovesAnimalKB, FOLParser.parse("Kills(Curiosity,Tuna)")))
  }
/* TODO: work on it
  def testJackKillsTunaFails() {
    assertTrue(!FOLResolution(KBFactory.lovesAnimalKB, FOLParser.parse("Kills(Jack,Tuna)")))
  }
*/
  def testEqualityAxiomsKBabcAEqualsCSucceeds() {
    assertTrue(FOLResolution(KBFactory.aBCEqualityKB(true),FOLParser.parse("A = C")))
  }

  def testEqualityNoAxiomsKBabcAEqualsCFails() {
    assertTrue(!FOLResolution(KBFactory.aBCEqualityKB(false),FOLParser.parse("A = C")))
  }
}
