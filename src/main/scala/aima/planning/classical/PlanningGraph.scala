package aima.planning.classical

import aima.commons.Utils

/** PLANNING-GRAPH, described in section 10.3
 *
 * @author Himanshu Gupta
 */
class PlanningGraph(problem: ClassicalPlanningProblem) {

  private var _currStateLevel = 0 //max current state level up

  private var stateLevels = initStateLevel0
  private var actionLevels = Map[Int,ActionLevel]()

  def expandGraph = {
    //generate next action level
    val actions = problem.actions.filter(_.preconditions.subsetOf(stateLevels(_currStateLevel).items)) ++
      stateLevels(_currStateLevel).items.map(Action.noOp(_)) //No-Ops
    actionLevels = actionLevels + (_currStateLevel -> new ActionLevel(actions,getActionMutexes(actions,stateLevels(_currStateLevel))))

    //generate next state level
    val literals = stateLevels(_currStateLevel).items ++ actions.flatMap(_.effects)
    stateLevels = stateLevels + (_currStateLevel+1 ->
                                   new StateLevel(literals, getLiteralMutexes(literals,Some(actionLevels(_currStateLevel)))))

    //increment currStateLevel
    _currStateLevel = _currStateLevel + 1
    
    this
  }
    
  def stateLevel(n: Int) = stateLevels(n)
  def actionLevel(n: Int) = actionLevels(n)

  def isLeveledOff: Boolean =
    if(_currStateLevel == 0) false
    else stateLevels(_currStateLevel) == stateLevels(_currStateLevel - 1)

  private def initStateLevel0 = {
    val allPositives = collectPositiveLiterals(problem.actions)
    val literals = problem.initState ++ (allPositives -- problem.initState).map(makeNegative(_))
    val mutexes = getLiteralMutexes(literals, None)
    Map[Int,StateLevel](0 -> new StateLevel(literals,mutexes))
  }

  private def getLiteralMutexes(literals: Set[Literal], prevLevel: Option[ActionLevel]): Set[(Literal,Literal)] = {

    def isMutex(x: Literal, y: Literal): Boolean =
      (x,y) match {
        //If the two are complementary
        case (_:PositiveLiteral,_:NegativeLiteral) if x.sentence == y.sentence => true
        case (_:NegativeLiteral,_:PositiveLiteral) if x.sentence == y.sentence => true
        //else
        case _ =>
          //if each pair of actions(from prev action level) achieving them is mutex
          prevLevel match {
            case None => false
            case Some(alevel) =>
              //find all pair of actions that achieve x and y
              def doAchieveXY(p: Action, q: Action): Boolean = {
                (p.effects.exists(_ == x) && q.effects.exists(_ == y)) ||
                (p.effects.exists(_ == y) && q.effects.exists(_ == x))
              }

              val ps = Utils.pairs(alevel.items,doAchieveXY)
              !ps.isEmpty &&
              ps.filter(
                _ match {
                  case (a1,a2) =>
                    !alevel.mutexes.exists(
                      _ match {
                        case (m1,m2) =>
                          ((a1 == m1) && (a2 == m2)) || ((a1 == m2) && (a2 == m1))
                      })
                }).isEmpty
          }
      }

    Utils.pairs(literals,isMutex)
  }

  private def getActionMutexes(actions: Set[Action], prevLevel: StateLevel): Set[(Action,Action)] = {
    
    def isMutex(x: Action, y: Action): Boolean = {
      isInconsistent(x.effects,y.effects) ||             //Inconsistent effects
      isInconsistent(x.preconditions,y.effects) ||       //Interference
      isInconsistent(x.effects,y.preconditions) ||       //Interference
      competingNeeds(x.preconditions,y.preconditions,prevLevel)    //Competing Needs
    }

    //Returns true if two sets contain complimentary literals
    def isInconsistent(xs: Set[Literal], ys: Set[Literal]): Boolean =
      xs.exists(x =>
        ys.exists(y =>
          (x,y) match {
            case (_:PositiveLiteral,_:NegativeLiteral) => x.sentence == y.sentence
            case (_:NegativeLiteral,_:PositiveLiteral) => x.sentence == y.sentence
            case _ => false
          }))

    def competingNeeds(xs: Set[Literal], ys: Set[Literal], prevLevel: StateLevel): Boolean =
      xs.exists(x =>
        ys.exists(y =>
          prevLevel.mutexes.exists(
            _ match {
              case (a,b) => ((a == x) && (b == y)) || ((a == y) && (b == x))
            })))

    Utils.pairs(actions,isMutex)
  }

  private def collectPositiveLiterals(actions: Set[Action]) : Set[Literal] =
    actions.flatMap(a =>
      a.preconditions.filter(_.isPositive) ++ a.effects.filter(_.isPositive))

  private def makeNegative(l: Literal): NegativeLiteral =
    (l: @unchecked) match {
      case x: PositiveLiteral => NegativeLiteral(x.sentence)
      case x: NegativeLiteral => x
    }
}

class Level[A](val items: Set[A], val mutexes: Set[(A,A)]) {

  //Checks that no two elements of given set are mutex
  def isConflictFree(that: Set[A]): Boolean = {
    def loop(that: List[A]): Boolean =
      that match {
        case x :: rest =>
          if(rest.exists(y =>
              mutexes.exists(m => (m == (x,y)) || (m == (y,x)))))
            false
          else loop(rest)
        case Nil => true
      }
    loop(that.toList)
  }
    

  override def equals(that: Any) =
    that match {
      case x: Level[A] => this.items == x.items
      case _ => false
    }
}
class StateLevel(literals: Set[Literal], ms: Set[(Literal,Literal)]) extends Level[Literal](literals,ms)
class ActionLevel(actions: Set[Action], ms: Set[(Action,Action)]) extends Level[Action](actions,ms)
