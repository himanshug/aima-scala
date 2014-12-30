package aima.planning.classical

import junit.framework._
import Assert._

class ClassicalPlanningProblemTest extends TestCase {

  //testing the "have cake and eat cake too" problem
  def testIt() {

    val haveCakeP = PositiveLiteral(new Atom("Have",List("Cake")))
    val haveCakeN = NegativeLiteral(new Atom("Have",List("Cake")))
    val eatenCakeP = PositiveLiteral(new Atom("Eaten",List("Cake")))
    val eatenCakeN = NegativeLiteral(new Atom("Eaten",List("Cake")))
    val eatCakeP = PositiveLiteral(new Atom("Eat",List("Cake")))
    val eatCakeN = NegativeLiteral(new Atom("Eat",List("Cake")))
    val bakeCakeP = PositiveLiteral(new Atom("Bake",List("Cake")))
    val bakeCakeN = NegativeLiteral(new Atom("Bake",List("Cake")))

    //actions
    val eatCakeA = Action("Eat(Cake)","","")
    val bakeCakeA = Action("Bake(Cake)","","")

    val p = ClassicalPlanningProblems.haveCakeAndEatCakeToo

    assertEquals(Set(haveCakeP),p.initState)
    assertEquals(Set(haveCakeP,eatenCakeP),p.goals)
    assertEquals(Set(eatCakeA,bakeCakeA),p.actions)
  }
}
