package aima.search.adversarial

/** MinimaxDecision, described in Fig 5.3
 *
 * @author Himanshu Gupta
 */
object MinimaxDecision {
  def apply[S,A](state: S, game: ZeroSumGame[S,A]): A = {
    val actionMinvalPairs = game.actions(state).map((a) => (a,MinValue(game.result(state,a),game)))
    //sort the pairs in descending order of MinValue
    val sorted = actionMinvalPairs.sort(_._2 > _._2)
    //take the pairs with highest MinValue(they can be more than 1 also)
    val bestPairs = sorted.takeWhile(sorted.head._2 == _._2)
    //chose randomly one from the best
    bestPairs(new scala.util.Random(new java.util.Random).nextInt(bestPairs.length))._1
  }

  private def MaxValue[S,A](state: S, game: ZeroSumGame[S,A]): Double =
    if (game.terminalTest(state)) game.utility(state)
    else
      game.actions(state).map(game.result(state,_)).foldLeft(Math.MIN_DOUBLE)( (v,s) => {
        val tmp = MinValue(s,game)
        if(v > tmp) v else tmp } )

  private def MinValue[S,A](state: S, game: ZeroSumGame[S,A]): Double =
    if (game.terminalTest(state)) game.utility(state)
    else
      game.actions(state).map(game.result(state,_)).foldLeft(Math.MAX_DOUBLE)( (v,s) => {
        val tmp = MaxValue(s,game)
        if(v < tmp) v else tmp } )
}

/** AlphaBetaSearch, described in Fig 5.7
 *
 * @author Himanshu Gupta
 */
object AlphaBetaSearch {
  def apply[S,A](state: S, game: ZeroSumGame[S,A]): A = {
    val actionMinvalPairs = game.actions(state).map((a) => 
                                  (a,MinValue(game.result(state,a),Math.MIN_DOUBLE,Math.MAX_DOUBLE,game)))
    //sort the pairs in descending order of MinValue
    val sorted = actionMinvalPairs.sort(_._2 > _._2)
    //take the pairs with highest MinValue(they can be more than 1 also)
    val bestPairs = sorted.takeWhile(sorted.head._2 == _._2)
    //chose randomly one from the best
    bestPairs(new scala.util.Random(new java.util.Random).nextInt(bestPairs.length))._1
  }

  private def MaxValue[S,A](state: S, alpha: Double,beta: Double, game: ZeroSumGame[S,A]): Double =
    if (game.terminalTest(state)) game.utility(state)
    else {
      def loop(states: List[S], v: Double, alpha: Double): Double =
        states match {
          case s :: rest => {
            val tmp = Math.max(v,MinValue(s,alpha,beta,game))
            if(tmp >= beta) tmp
            else loop(rest, tmp, Math.max(alpha,tmp))
          }
          case Nil => v
        }

      loop(game.actions(state).map(game.result(state,_)),Math.MIN_DOUBLE,alpha)
    }

  private def MinValue[S,A](state: S, alpha: Double, beta: Double, game: ZeroSumGame[S,A]): Double =
    if (game.terminalTest(state)) game.utility(state)
    else {
      def loop(states: List[S], v: Double, beta: Double): Double =
        states match {
          case s :: rest => {
            val tmp = Math.min(v,MaxValue(s,alpha,beta,game))
            if(tmp <= alpha) tmp
            else loop(rest, tmp, Math.min(beta,tmp))
          }
          case Nil => v
        }

      loop(game.actions(state).map(game.result(state,_)),Math.MAX_DOUBLE,beta)
    }
}
