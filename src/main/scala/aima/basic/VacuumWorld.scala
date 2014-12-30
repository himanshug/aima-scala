package aima.basic

import scala.util.Random

/* This file containts code for Vacuum World environment as described in *
 * in Fig 2.2 and various agents in it.
 *
 * @author Himanshu Gupta
 */
object VacuumWorld {

  type Percept = (String,String) //("A"/"B" , "Clean"/"Dirty")
  type Action = String           // "Suck" / "Left" / "Right"
  type State = Map[String,String]

  /* ----------------- Trivial Vacuum World Environment ------------------- */
  class TrivialVacuumEnvironment[T <: VacuumWorldAgent](private var statusA:String,
                                                        private var statusB:String) 
  extends Environment[T , Percept, Action] {
    
    def this() = {
      this( if(new Random().nextBoolean) "Clean" else "Dirty",
           if(new Random().nextBoolean) "Clean" else "Dirty")
    }
    
    override def executeAction(agent: T, action: Option[Action]) {
      action match {
        case None => agent.die()
        case Some("Right") =>
          agent location_= "B"
        agent performance_= agent.performance - 1
        case Some("Left") =>
          agent location_= "A"
        agent performance_= agent.performance - 1
        case Some("Suck") =>
          setStatusClean(agent.location)
        agent performance_= agent.performance + 10
        case Some(x) => throw new IllegalArgumentException("Invalid action. " + x)
      }
    }

    override def getPerceptSeenBy(agent: T) =
      agent.location match {
        case "A" => ("A",statusA)
          case "B" => ("B",statusB)
            case location => throw new IllegalArgumentException("Unknown location. " + location)
      }
    
    private def setStatusClean(location: String) {
      location match {
        case "A" => statusA = "Clean"
        case "B" => statusB = "Clean"
        case _ => throw new IllegalArgumentException("Unknown location. " + location)
      }
    }
  }

  //=============================== Agents ===================================

  abstract class VacuumWorldAgent(var location: String, var performance: Int)
           extends Agent[Percept,Action]

  /* A Table-Driven-Agent based on the table described in Fig 2.3 */
  class TableDrivenVacuumAgent(l:String)
  extends VacuumWorldAgent(l,0) with TableDrivenAgent[Percept,Action] {
    override protected val table = {
      var tmpTable:Map[List[Percept],Action] = Map();

      //Level1: 4 states
      tmpTable += List(("A","Clean")) -> "Right"
      tmpTable += List(("A","Dirty")) -> "Suck"
      tmpTable += List(("B","Clean")) -> "Left"
      tmpTable += List(("B","Dirty")) -> "Suck"
      
      //Level2: 4 X 4 states
      tmpTable += List(("A","Clean"),("A","Clean")) -> "Right"
      tmpTable += List(("A","Clean"),("A","Dirty")) -> "Suck"
      tmpTable += List(("A","Dirty"),("A","Clean")) -> "Right"
      tmpTable += List(("A","Dirty"),("A","Dirty")) -> "Suck"

      tmpTable += List(("B","Clean"),("B","Clean")) -> "Left"
      tmpTable += List(("B","Clean"),("B","Dirty")) -> "Suck"
      tmpTable += List(("B","Dirty"),("B","Clean")) -> "Left"
      tmpTable += List(("B","Dirty"),("B","Dirty")) -> "Suck"

      tmpTable += List(("A","Clean"),("B","Clean")) -> "Left"
      tmpTable += List(("A","Dirty"),("B","Clean")) -> "Left"
      tmpTable += List(("A","Clean"),("B","Dirty")) -> "Suck"
      tmpTable += List(("A","Dirty"),("B","Dirty")) -> "Suck"

      tmpTable += List(("B","Clean"),("A","Clean")) -> "Right"
      tmpTable += List(("B","Dirty"),("A","Clean")) -> "Right"
      tmpTable += List(("B","Clean"),("A","Dirty")) -> "Suck"
      tmpTable += List(("B","Dirty"),("A","Dirty")) -> "Suck"

      tmpTable
    }
  }

  /* Reflex-Vacuum-Agent as described in Fig 2.8 */
  class ReflexVacuumAgent(l: String) extends VacuumWorldAgent(l,0) {
    override def agentProgram(percept: Percept) = 
      percept match {
        case (_,"Dirty") => Some("Suck")
        case ("A",_) => Some("Right")
        case ("B",_) => Some("Left")
        case _ => throw new IllegalArgumentException("Invalid Percept. " + percept)
      }
  }

  import simplerule._

  /* A vacuum world agent based on Simple-Reflex-Agent */
  class SimpleReflexVacuumAgent(l: String) 
  extends VacuumWorldAgent(l,0) with SimpleReflexAgent[State,Percept,Action] {
    
    override protected val rules = {
      
      // returns List(Rule(EqualCondition,Action))
      def ruleGen(key: String, value: String, action: Action): List[Rule[State,Option[Action]]] =
        List(new Rule(
              new EqualCondition( (s: State) => (s.contains(key) && s.get(key) == Some(value))),
              Some(action)))

      var tmpRules: List[Rule[State,Option[Action]]] = Nil
      tmpRules ++= ruleGen("status","Dirty","Suck")
      tmpRules ++= ruleGen("location","A","Right")
      tmpRules ++= ruleGen("location","B","Left")
      tmpRules
    }

    override protected def interpretInput(percept: Percept) =
      Map("location" -> percept._1, "status" -> percept._2)
  }

  /* A vacuum world agent based on Model-Based-Reflex-Agent */
  class ReflexVacuumAgentWithState(l: String)
  extends VacuumWorldAgent(l,0) with ModelBasedReflexAgent[State,Percept,Action] {
    
    override protected var state: State = Map()

    override protected val rules = {
      
      // returns EqualCondition
      def eqCondGen(key: String, value: String) =
        new EqualCondition( (s: State) => (s.contains(key) && s.get(key) == Some(value)))

      //returns List(Rule(EqualCondition,Action))
      def ruleGen(key: String, value: String, action: Action): List[Rule[State,Option[Action]]] =
        List(new Rule(eqCondGen(key,value),Some(action)))

      var tmpRules: List[Rule[State,Option[Action]]] = Nil
      tmpRules ++= List[Rule[State,Option[Action]]](new Rule(new AndCondition
                                                             (eqCondGen("statusA","Clean"),
                                                              eqCondGen("statusB","Clean")),None))
      tmpRules ++= ruleGen("status", "Dirty", "Suck")
      tmpRules ++= ruleGen("location", "A", "Right")
      tmpRules ++= ruleGen("location", "B", "Left")
      tmpRules
    }

    override protected def updateState(state: State, action: Option[Action], percept: Percept) =
      state + ("location" -> percept._1,
               "status" -> percept._2,
               "status" + percept._1 -> percept._2)
  }
}
