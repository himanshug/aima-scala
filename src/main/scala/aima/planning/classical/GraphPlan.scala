package aima.planning.classical

import aima.commons.Utils
import aima.search.uninformed.DepthFirstTreeSearch
import aima.search.Success

/** GRAPHPLAN algorithm, described in Fig 10.9
 *
 * @author Himanshu Gupta
 */
object GraphPlan {
  def apply(problem: ClassicalPlanningProblem) = {
    
    def loop(tl: Int, graph: PlanningGraph): Option[List[Set[Action]]] =
      if (problem.goals.subsetOf(graph.stateLevel(tl).items) &&
          graph.stateLevel(tl).isConflictFree(problem.goals)) {
        val sol = extractSolution(graph,problem,tl)
        if(sol != None) sol
        else {
          if (graph.isLeveledOff) None
          else loop(tl+1, graph.expandGraph)
        }
      }
      else {
        if (graph.isLeveledOff) None
        else loop(tl+1, graph.expandGraph)
      }

    loop(0,new PlanningGraph(problem))
  }

  def extractSolution(graph: PlanningGraph, cpp: ClassicalPlanningProblem, n: Int) = {
    //Formulate as Search Problem
    val sp = new SearchProblem(graph,cpp,n)
    DepthFirstTreeSearch(sp) match {
      case Success(result) => Some(result.reverse)
      case _ => None
    }
  }
}


class SearchProblem(planningGraph: PlanningGraph, planningProblem: ClassicalPlanningProblem, lastLevelNum: Int)
extends aima.search.Problem[(Set[Literal],Int),Set[Action]] {

  override def initialState = (planningProblem.goals,lastLevelNum)

  override def goalTest(s: (Set[Literal],Int)) =
    s._1.filter(_.isPositive).subsetOf(planningProblem.initState)

  override def actions(s: (Set[Literal],Int)) =
    s match {
      case (literals,n) if n > 0 =>
        //Find Set of Actions from An-1
        val actionLevel = planningGraph.actionLevel(n-1)
        var as = actionLevel.items
        //Find the subset, whose effects cover the literals
        //take the only actions whose effects contribute to
        //literals
        as = as.filter(a => !(a.effects ** literals).isEmpty)

        //find all subsets of above and filter the ones those
        //not satisfying the literals OR contain conflicts
        Utils.subsets(as).filter(s =>
            literals.subsetOf(s.flatMap(_.effects))
            &&
            actionLevel.isConflictFree(s)
        ).toList
    }

  override def result(s: (Set[Literal],Int), a: Set[Action]) = (a.flatMap(_.preconditions),s._2 - 1)
    
  override def stepCost(from: (Set[Literal],Int), action: Set[Action], to: (Set[Literal],Int)): Double = 0.0
  override def estimatedCostToGoal(from: (Set[Literal],Int)): Double = 0.0
}
