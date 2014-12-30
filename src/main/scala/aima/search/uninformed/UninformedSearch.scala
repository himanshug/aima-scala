package aima.search.uninformed

import aima.search._

/** Tree-Search, described in Fig 3.7
 *
 * @author Himanshu Gupta
 */
object TreeSearch {
  def apply[S,A](problem: Problem[S,A], frontier: Queue[Node[S,A]]) = {
    def loop(frontier:Queue[Node[S,A]]): SearchResult[List[A]] =
      frontier.removeFirst match {
        case None => Failure()
        case Some(node) if problem.goalTest(node.state) => 
          //println(node.state.toString())
          Success(node.solution)
        case Some(node) => {
          problem.actions(node.state).foreach( (a:A) => frontier.insert(Node.childNode(problem,node,a)))
          loop(frontier)
        }
      }
    
    loop(frontier.insert(Node(problem.initialState)))
  }
}
 
/** Graph-Search, described in Fig 3.7
 *
 * @author Himanshu Gupta
 */
object GraphSearch {
  def apply[S,A](problem: Problem[S,A], frontier: Queue[Node[S,A]]) = {

    //TODO: make "explored" a hash based data structure so that lookup
    //is O(1)
    def loop(frontier:Queue[Node[S,A]], explored: List[S]): SearchResult[List[A]] =
      frontier.removeFirst match {
        case None => Failure()
        case Some(node) if problem.goalTest(node.state) => 
          //println(node.state.toString())
          Success(node.solution)
        case Some(node) => 
          if(explored.exists(_ == node.state)) 
             loop(frontier,explored)
          else {
            problem.actions(node.state).foreach((a:A) => frontier.insert(Node.childNode(problem,node,a)))
            loop(frontier, node.state :: explored)
          }
      }
    
    loop(frontier.insert(Node(problem.initialState)),Nil)
  }
}

/** Breadth-First-Search based on Tree-Search
 *
 * @author Himanshu Gupta
 */
object BreadthFirstTreeSearch {
  def apply[S,A](problem: Problem[S,A]) =
    TreeSearch(problem, new FifoQueue[Node[S,A]]())
}

/** Breadth-First-Search based on Graph-Search
 *
 * @author Himanshu Gupta
 */
object BreadthFirstGraphSearch {
  def apply[S,A](problem: Problem[S,A]) =
    GraphSearch(problem, new FifoQueue[Node[S,A]]())
}

/** Depth-First-Search based on Tree-Search
 *
 * @author Himanshu Gupta
 */
object DepthFirstTreeSearch {
  def apply[S,A](problem: Problem[S,A]) =
    TreeSearch(problem, new LifoQueue[Node[S,A]]())
}

/** Depth-First-Search based on Graph-Search
 *
 * @author Himanshu Gupta
 */
object DepthFirstGraphSearch {
  def apply[S, A](problem: Problem[S,A]) =
    GraphSearch(problem, new LifoQueue[Node[S,A]]())
}

/** Uniform-Cost-Search, described in Fig 3.14
 *
 * @author Himanshu Gupta
 */
object UniformCostSearch {
  def apply[S, A](problem: Problem[S,A]) =
    GraphSearch(problem, new PriorityQueue[Node[S,A]](
      new Ordering[Node[S,A]] {
        def compare(x: Node[S,A], y: Node[S,A]) =
          y.pathCost.compare(x.pathCost)
      }))
}

/** Depth-Limited-Search, described in Fig 3.17
 *
 * @author Himanshu Gupta
 */
object DepthLimitedSearch {
  def apply[S, A](problem: Problem[S,A], limit: Int) =
    recursiveDLS(Node[S,A](problem.initialState),problem,limit)

  /* Recursive-DLS, described in Fig 3.17 */
  private def recursiveDLS[S, A](node: Node[S,A], problem: Problem[S,A], limit: Int): SearchResult[List[A]] = {
    if (problem.goalTest(node.state)) Success(node.solution) //success
    else {
      if (node.depth == limit) CutOff() //cut-off limit reached
      else {
        def loop(nodes: List[Node[S,A]], cutoffOccured: Boolean): SearchResult[List[A]] = 
          nodes match {
            case Nil => if(cutoffOccured) CutOff() else Failure()
            case n :: rest => 
              recursiveDLS(n,problem,limit) match {
                case Failure() => loop(rest,cutoffOccured)
                case CutOff() => loop(rest,true)
                case Success(n) => Success(n)
              }
          }
        loop(problem.actions(node.state).map(Node.childNode(problem,node,_)), false)
      }
    }
  }
}

/** Iterative-Deepening-Search, described in Fig 3.18
 *
 * @author Himanshu Gupta
 */
object IterativeDeepeningSearch {
  def apply[S, A](problem: Problem[S,A]) = {
    def loop(depth: Int): SearchResult[List[A]] =
      DepthLimitedSearch(problem,depth) match {
        case CutOff() => loop(depth + 1)
        case Failure() => Failure()
        case Success(actions) => Success(actions)
      }
    loop(0)
  }
}

