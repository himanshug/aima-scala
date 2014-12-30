package aima.search.online

import junit.framework._
import Assert._

import aima.search.{In,Go,LocationMap}

class OnlineDFSTest extends TestCase {

  def testAIMA2eFig4_18() {
    var result:List[Go[String]] = List()
    val map = new LocationMap[String]()
    map.addPath("1,1", "1,2", 1.0);
    map.addPath("1,1", "2,1", 1.0);
    map.addPath("2,1", "3,1", 1.0);
    map.addPath("2,1", "2,2", 1.0);
    map.addPath("3,1", "3,2", 1.0);
    map.addPath("2,2", "2,3", 1.0);
    map.addPath("3,2", "3,3", 1.0);
    map.addPath("2,3", "1,3", 1.0);

    val problem = new OnlineSearchMapProblem[String](map,In("3,3"))
    val agent = new OnlineDFSMapAgent[String](problem,In("1,1"))

    val env = new MapEnvironment[OnlineDFSMapAgent[String],String]()

    env.registerView( _ match {
                          case Some(a) => result = a :: result
                          case None => ;})
    env.addAgent(agent)
    env.stepUntilNoOp()
    
    assertEquals(result.reverse,List(Go("2,1"), Go("2,2"), Go("2,1"), Go("3,1"), Go("2,1"), Go("1,1"), 
    		Go("1,2"), Go("1,1"), Go("1,2"), Go("1,1"), Go("2,1"), Go("3,1"), Go("3,2"), Go("3,3")));
  }
}

class LRTAStarTest extends TestCase {

  private def map = {
    val aMap = new LocationMap[String]()
    aMap.addPath("A", "B", 4.0);
    aMap.addPath("B", "C", 4.0);
    aMap.addPath("C", "D", 4.0);
    aMap.addPath("D", "E", 4.0);
    aMap.addPath("E", "F", 4.0);

    aMap.addStraightLineDistance("F","F",0);
    aMap.addStraightLineDistance("F","A",1);
    aMap.addStraightLineDistance("F","B",1);
    aMap.addStraightLineDistance("F","C",1);
    aMap.addStraightLineDistance("F","D",1);
    aMap.addStraightLineDistance("F","E",1);
    aMap
  }

  def testAlreadyAtgoal() {
    var result:List[Go[String]] = List()

    val problem = new OnlineSearchMapProblem[String](map,In("A"))
    val agent = new LRTAStarMapAgent[String](problem,In("A"))

    val env = new MapEnvironment[LRTAStarMapAgent[String],String]()

    env.registerView( _ match {
                          case Some(a) => result = a :: result
                          case None => ;})
    env.addAgent(agent)
    env.stepUntilNoOp()
    assertEquals(result,List())
  }

  def testNormalSearch() {
    var result:List[Go[String]] = List()

    val problem = new OnlineSearchMapProblem[String](map,In("F"))
    val agent = new LRTAStarMapAgent[String](problem,In("A"))

    val env = new MapEnvironment[LRTAStarMapAgent[String],String]()

    env.registerView( _ match {
                          case Some(a) => result = a :: result
                          case None => ;})
    env.addAgent(agent)
    env.stepUntilNoOp()
    assertEquals(result.reverse,List(Go("B"), Go("A"), Go("B"), Go("C"), Go("B"), Go("C"), 
    		Go("D"), Go("E"), Go("D"), Go("C"), Go("B"), Go("A"), Go("B"), Go("C"), Go("D"), Go("E"), Go("F")))
  }

  def testNoPath() {
    val aMap = map
    aMap.addStraightLineDistance("G","G",0);
    aMap.addStraightLineDistance("G","A",1);
    aMap.addStraightLineDistance("G","B",1);
    aMap.addStraightLineDistance("G","C",1);
    aMap.addStraightLineDistance("G","D",1);
    aMap.addStraightLineDistance("G","E",1);
    aMap.addStraightLineDistance("G","F",1);
    
    var result:List[Go[String]] = List()

    val problem = new OnlineSearchMapProblem[String](aMap,In("G"))
    val agent = new LRTAStarMapAgent[String](problem,In("A"))

    val env = new MapEnvironment[LRTAStarMapAgent[String],String]()

    env.registerView( _ match {
                          case Some(a) => result = a :: result
                          case None => ;})
    env.addAgent(agent)
    env.step(14) //or else it'll run forever

    assertEquals(result.reverse,List(Go("B"), Go("A"), Go("B"), Go("C"), Go("B"), Go("C"), Go("D"),
    		Go("E"), Go("D"), Go("C"), Go("B"), Go("A"), Go("B"), Go("C")))
  }
}
