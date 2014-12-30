package aima.search.local

import aima.search.{SearchResult,Success,Failure,NonDeterministicProblem}

/* NOTE: Though AndOrGraphSearch belongs to package local, but it is *NOT* a
 * local search algorithm. I just could not think of a better place to
 * put it yet.
 */


/** And-Or-Graph-Search, described in Fig 4.11
 *
 * @author Himanshu Gupta
 */
object AndOrGraphSearch {
  def apply[S,A](problem: NonDeterministicProblem[S,A]): SearchResult[Plan[S,A]] =
    OrSearch(problem.initialState,problem,List[S]())

  //Returns Failure/Success(Plan)
  private def OrSearch[S,A](state: S, problem: NonDeterministicProblem[S,A], path: List[S]): SearchResult[Plan[S,A]] = {
     if (problem.goalTest(state))
      Success(new Plan[S,A](state,None))
    else {
      if (path.exists(_ == state)) Failure()
      else {
        def loop(actions: List[A]): SearchResult[Plan[S,A]] =
          actions match {
            case action :: rest =>
              AndSearch(problem.results(state,action),problem, state :: path,state,action) match {
                case Success(x) => Success(x)
                case _ => loop(rest)
              }
            case Nil => Failure()
          }
        loop(problem.actions(state))
      }
    }
  }

  private def AndSearch[S,A](states: List[S], problem: NonDeterministicProblem[S,A],path: List[S],prevState: S, prevAction: A): SearchResult[Plan[S,A]] = {
    if(states.length == 0)
      throw new IllegalStateException("From " + prevState + " action " + prevAction + " results in no states.")
    
    var result = new Plan(prevState,Some(prevAction))
    def loop(states: List[S]): SearchResult[Plan[S,A]] = {
      states match {
        case state :: rest =>
          OrSearch(state,problem,path) match {
            case Success(x) => result.addChild(x); loop(rest)
            case _ => Failure()
          }
        case Nil => Success(result)
      }
    }
    loop(states)
  }
}


/*
 * The And-Or-Graph-Search returns a Plan tree as solution.
 *
 * For example, the Plan tree for solution described
 * in Fig 4.10 looks like following(numbers denote the
 * corresponding states shown in the figure for that number)..
 * 
 *             (1,Some(Suck))
 *                |
 *    |-----------------------|
 * (7,None)                (5,Some(Right))
 *                            |
 *                         (6,Some(Suck))
 *                            |
 *                         (8,None)
 *
 * @author Himanshu Gupta
 *
 */
class Plan[S,A](s: S, a: Option[A]) {
  private val children = new scala.collection.mutable.Queue[Plan[S,A]]()

  def addChild(plan: Plan[S,A]) { children.enqueue(plan) }
  def isLeaf = children.isEmpty

  def state = s
  def action = a

  //Only for the testing purpose, for any computation
  //Plan instance itself should be used
  override def toString() = {
    var result = rawActionString(a)
    if(children.length == 1)
      result = result + " " + children(0).toString()
    else {
      if(children.length > 0) {
        for(x <- children) {
          result = result + " IF " + x.state + " THEN [" + x.toString() + "] ELSE "
        }
        result = result.substring(0,result.length-6)
      }
    }
    result
  }

  private def rawActionString(a: Option[A]):String =
    a match {
      case Some(x) => x.toString()
      case None => "NoOp"
    }
}
