package aima.search.informed

import aima.search._
import aima.search.uninformed.{GraphSearch, TreeSearch}

/** Greedy-Best-First-Search, described in Section 3.5.1
 *
 * @author Himanshu Gupta
 */
object GreedyBestFirstSearch {
  def apply[S,A](problem: Problem[S,A]) =
    GraphSearch(problem, new PriorityQueue[Node[S,A]](
      new Ordering[Node[S,A]] {
        def compare(x: Node[S,A], y: Node[S,A]) =
          GreedyBestFirstHeuristic(y,problem).compare(GreedyBestFirstHeuristic(x,problem))
      }))

  private def GreedyBestFirstHeuristic[S,A](node: Node[S,A],problem: Problem[S,A]) =
    problem.estimatedCostToGoal(node.state) //h(n)
}

/* A* Search, described in section 3.5.2
 *
 * @author Himanshu Gupta
 */
object AStarSearch {
  def apply[S, A](problem: Problem[S,A]) =
    GraphSearch(problem, new PriorityQueue[Node[S,A]](
      new Ordering[Node[S,A]] {
        def compare(x: Node[S,A], y: Node[S,A]) =
          AStarHeuristic(y,problem).compare(AStarHeuristic(x,problem))
      }))

  private def AStarHeuristic[S,A](node: Node[S,A],problem: Problem[S,A]) =
    node.pathCost + problem.estimatedCostToGoal(node.state) //f(n) = g(n) + h(n)
}

/* Recursive-Best-First-Search, described in Fig 3.26
 *
 * @author Himanshu Gupta
 */
object RecursiveBestFirstSearch {

  def apply[S, A](problem: Problem[S,A]): SearchResult[List[A]] =
    RBFS(problem,RBFSNode(problem,problem.initialState),Infinity) match {
      case Success(x) => Success(x)
      case _ => Failure()
    }

  private val Infinity = Math.MAX_DOUBLE
  private final case class RBFSFailure[A](fVal: Double) extends Failure[A]
  
  //RBFS-Node is a special purpose node that stores node's fVal in a variable 
  //along with what is stored in a regular Node
  private final class RBFSNode[S,A](state: S, parent: Option[Node[S,A]], action: Option[A], depth: Int, pathCost: Double, var fVal: Double)
        extends Node[S,A](state,parent,action,depth,pathCost) {
          override def toString = "(" + state.toString + " , " + fVal.toString + ")"
        } 
  private object RBFSNode {
    def apply[S,A](problem: Problem[S,A],state: S) =
      new RBFSNode[S,A](state,None,None,0,0,problem.estimatedCostToGoal(state))

    def childNode[S,A](problem: Problem[S,A],parent: RBFSNode[S,A],action: A) = {
      val state = problem.result(parent.state,action)
      val pathCost = parent.pathCost + problem.stepCost(parent.state,action,state)
      val fVal = pathCost + problem.estimatedCostToGoal(state)
      new RBFSNode[S,A](state,Some(parent),Some(action),parent.depth+1,pathCost,fVal)
    }
  }

  /* RBFS, described in Fig 3.26
   * returns Success or RBFSFailure AND a new f-cost limit
   */
  private def RBFS[S, A](problem: Problem[S,A], node: RBFSNode[S,A], fLimit: Double): SearchResult[List[A]] = {
    if(problem.goalTest(node.state)) Success(node.solution)
    else {
      val successors = problem.actions(node.state).map((a:A)=>RBFSNode.childNode(problem,node,a))
      if(successors.length == 0) RBFSFailure(Infinity)
      else {
        //update fValue of any node from previous search
        successors.foreach((n) => n.fVal = Math.max(n.fVal,node.fVal))

        //storing the nodes in a PriorityQueue in increasing order
        //of fVal for easy access of best and next best nodes
        val successorsPq = new PriorityQueue[RBFSNode[S,A]](
          new Ordering[RBFSNode[S,A]] {
            def compare(x: RBFSNode[S,A], y: RBFSNode[S,A]) = y.fVal.compare(x.fVal)
          })
        successorsPq.insertAll(successors)

        def loop: SearchResult[List[A]] = {
          val Some(bestNode) = successorsPq.nth(0)
          if(bestNode.fVal > fLimit) RBFSFailure(bestNode.fVal)
          else {
            val alternative_fVal =
              successorsPq.nth(1) match {
                case Some(n) => n.fVal
                case None => bestNode.fVal
              }

            RBFS(problem,bestNode,Math.min(fLimit, alternative_fVal)) match {
              case Success(a) => Success(a)
              case RBFSFailure(f) =>
                //remove the best node, reset its fVal and re-insert it to
                //the PriorityQueue, removal-insertion is necessary so
                //that at insert this node is placed according to its new
                //fVal in the PriorityQueue
                val Some(currentBest) = successorsPq.removeFirst
                currentBest.fVal = f
                successorsPq.insert(currentBest)
                loop
              case _ => throw new IllegalStateException("This can not happen.")
            }
          }
        }
        loop
      }
    }
  }
}
