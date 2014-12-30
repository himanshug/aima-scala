package aima.planning.classical

import junit.framework._
import Assert._

class PlanningGraphTest extends TestCase {

  //tests based on planning graph of "have cake and ear too" problem
  def testIt() {

    val haveCakeP: Literal = PositiveLiteral(new Atom("Have",List("Cake")))
    val haveCakeN: Literal = NegativeLiteral(new Atom("Have",List("Cake")))
    val eatenCakeP: Literal = PositiveLiteral(new Atom("Eaten",List("Cake")))
    val eatenCakeN: Literal = NegativeLiteral(new Atom("Eaten",List("Cake")))
    val eatCakeP: Literal = PositiveLiteral(new Atom("Eat",List("Cake")))
    val eatCakeN: Literal = NegativeLiteral(new Atom("Eat",List("Cake")))
    val bakeCakeP: Literal = PositiveLiteral(new Atom("Bake",List("Cake")))
    val bakeCakeN: Literal = NegativeLiteral(new Atom("Bake",List("Cake")))

    //actions
    val eatCakeA = Action("Eat(Cake)","","")
    val bakeCakeA = Action("Bake(Cake)","","")
    
    //Planning Graph
    var pGraph = new PlanningGraph(ClassicalPlanningProblems.haveCakeAndEatCakeToo)
    assertTrue(!pGraph.isLeveledOff)
    
    val s0 = pGraph.stateLevel(0)
    assertEquals(Set(haveCakeP,eatenCakeN),s0.items)
    assertEquals(Set.empty,s0.mutexes)


    pGraph = pGraph.expandGraph //A0 and S1 generated
    assertTrue(!pGraph.isLeveledOff)
    val a0 = pGraph.actionLevel(0)
    assertEquals(Set(eatCakeA,
                     Action.noOp(haveCakeP),
                     Action.noOp(eatenCakeN)),
                 a0.items)
    assertTrue(matchUnorderedPairs(a0.mutexes,
                               Set((Action.noOp(haveCakeP),eatCakeA),
                                   (Action.noOp(eatenCakeN),eatCakeA))))

    val s1 = pGraph.stateLevel(1)
    assertEquals(Set(haveCakeP,haveCakeN,eatenCakeP,eatenCakeN),s1.items)

    assertTrue(matchUnorderedPairs(s1.mutexes,
                                   Set((eatenCakeN,eatenCakeP),
                                       (haveCakeN,haveCakeP),
                                       (haveCakeP,eatenCakeP),
                                       (haveCakeN,eatenCakeN))))

    pGraph = pGraph.expandGraph //A1 and S2 generated
    assertTrue(pGraph.isLeveledOff)
    val a1 = pGraph.actionLevel(1)
    assertEquals(Set(eatCakeA,bakeCakeA,
                     Action.noOp(haveCakeP),
                     Action.noOp(haveCakeN),
                     Action.noOp(eatenCakeP),
                     Action.noOp(eatenCakeN)),
                 a1.items)
    assertTrue(matchUnorderedPairs(a1.mutexes,
                                   Set((Action.noOp(haveCakeN),bakeCakeA),
                                       (Action.noOp(haveCakeP),bakeCakeA),
                                       (Action.noOp(eatenCakeN),bakeCakeA),
                                       (bakeCakeA,eatCakeA),
                                       (Action.noOp(haveCakeP),Action.noOp(haveCakeN)),
                                       (Action.noOp(eatenCakeP),Action.noOp(eatenCakeN)),
                                       (Action.noOp(haveCakeN),Action.noOp(eatenCakeN)),
                                       (Action.noOp(haveCakeP),Action.noOp(eatenCakeP)),
                                       (Action.noOp(haveCakeP),eatCakeA),
                                       (Action.noOp(haveCakeN),eatCakeA),
                                       (Action.noOp(eatenCakeP),eatCakeA),
                                       (Action.noOp(eatenCakeN),eatCakeA))))
    val s2 = pGraph.stateLevel(2)
    assertEquals(Set(haveCakeP,
                     haveCakeN,
                     eatenCakeP,
                     eatenCakeN),
                 s2.items)
    assertTrue(matchUnorderedPairs(s2.mutexes,
                                   Set((eatenCakeN,eatenCakeP),
                                       (haveCakeN,haveCakeP),
                                       (eatenCakeN,haveCakeN))))
  }

  private def matchUnorderedPairs[A](p1: Set[(A,A)],p2: Set[(A,A)]): Boolean = {
    //println("Expected: " + p2)
    //println("Actual: " + p1)
    p1.forall(
      _ match {
        case (p1x,p1y) =>
          p2.exists(_ match {
            case (p2x,p2y) =>
              ((p1x == p2x) && (p1y == p2y)) ||
              ((p1x == p2y) && (p1y == p2x))
          })
      }) && (p1.size == p2.size)
  }
}
