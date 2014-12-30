package aima.planning.classical

import junit.framework._
import Assert._

class GraphPlanTest extends TestCase {


  def testHaveCakeAndEatCakeToo() {
    //the unsolvable goal -- Destroy(Cake)
    assertEquals(None,GraphPlan(ClassicalPlanningProblems.haveCakeAndEatCakeToo("Destroy(Cake)")))

    //solvable standard problem
    assertEquals(List(Set(Action("Eat(Cake)","","")),
                      Set(Action("Bake(Cake)","",""),
                          Action.noOp(PositiveLiteral(new Atom("Eaten",List("Cake")))))),
                 GraphPlan(ClassicalPlanningProblems.haveCakeAndEatCakeToo).get)
  }

  def testSpareTire() {
    //the unsolvable goal -- At(Spare,Axle) & At(Flat,Axle)
    assertEquals(None,GraphPlan(ClassicalPlanningProblems.spareTire("At(Spare,Axle) & At(Flat,Axle)")))
    
    //solvable standard problem
    assertEquals(List(Set(Action("Remove(Flat,Axle)","",""),
                          Action("Remove(Spare,Trunk)","",""),
                          Action.noOp(NegativeLiteral(new Atom("At",List("Spare","Axle"))))),
                      Set(Action("PutOn(Spare,Axle)","",""))),
                 GraphPlan(ClassicalPlanningProblems.spareTire).get)
  }
}
