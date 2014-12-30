package aima.search.adversarial

import junit.framework._
import Assert._

class MinimaxDecisionTest extends TestCase {

  private val game = new TicTacToeGame()
  private val initState = game.initialState
  private val X = 'X'
  private val O = 'O'

  /* Testing the best action at following state
   * - - -
   * - - -
   * - - -
   */
  def testInitState() {
    val action = MinimaxDecision(game.initialState,game)    
    val allPairs = (for(x <- 0 to 2; y <- 0 to 2) yield (x,y)).toList
    //all the states, where there is just one X and all other boxes are empty,
    //have MinValue = 0.5... so all moves are OK to begin with
    assertTrue(allPairs.exists(_ == action))
  }

  /* Testing the best action at following state,
   * - - X
   * - O -
   * - - -
   */
  def test1() {
    val board = new Array[Array[Char]](3,3)
    board(2)(2) = X
    board(1)(1) = O
    val state = (ZeroSumGame.Max,board)
    
    val action = MinimaxDecision(state,game)
    val bestActions = List((1,0),(2,0),(0,1),(2,1),(0,2),(1,2),(0,0))
    assertTrue(bestActions.exists(_ == action))
  }

  /* Testing the best action at following state,
   * - O X
   * X O -
   * - - -
   *
   * Resulting action should save the game by placing
   * X at (1,0)
   */
  def test2() {
    val board = new Array[Array[Char]](3,3)
    board(2)(2) = X
    board(1)(1) = O
    board(0)(1) = X
    board(1)(2) = O
    val state = (ZeroSumGame.Max,board)
    
    val action = MinimaxDecision(state,game)
    assertEquals(action,(1,0))
  }

  /* Testing the best action at following state,
   * O - O
   * - X -
   * X X O
   *
   * Resulting action should win the game by placing
   * X at (1,2)
   */
  def test3() {
    val board = new Array[Array[Char]](3,3)
    board(2)(2) = O
    board(0)(2) = O
    board(1)(1) = X
    board(0)(0) = X
    board(1)(0) = X
    board(2)(0) = O
    val state = (ZeroSumGame.Max,board)
    
    val action = MinimaxDecision(state,game)
    assertEquals(action,(1,2))
  }
}

class AlphaBetaSearchTest extends TestCase {

  private val game = new TicTacToeGame()
  private val initState = game.initialState
  private val X = 'X'
  private val O = 'O'

  /* Testing the best action at following state
   * - - -
   * - - -
   * - - -
   */
  def testInitState() {
    val action = AlphaBetaSearch(game.initialState,game)
    val allPairs = (for(x <- 0 to 2; y <- 0 to 2) yield (x,y)).toList
    //all the states, where there is just one X and all other boxes are empty,
    //have MinValue = 0.5... so all moves are OK to begin with
    assert(allPairs.exists(_ == action))
  }

  /* Testing the best action at following state,
   * - - X
   * - O -
   * - - -
   */
  def test1() {
    val board = new Array[Array[Char]](3,3)
    board(2)(2) = X
    board(1)(1) = O
    val state = (ZeroSumGame.Max,board)
    
    val action = AlphaBetaSearch(state,game)
    val bestActions = List((1,0),(2,0),(0,1),(2,1),(0,2),(1,2),(0,0))
    assert(bestActions.exists(_ == action))
  }

  /* Testing the best action at following state,
   * - O X
   * X O -
   * - - -
   *
   * Resulting action should save the game by placing
   * X at (1,0)
   */
  def test2() {
    val board = new Array[Array[Char]](3,3)
    board(2)(2) = X
    board(1)(1) = O
    board(0)(1) = X
    board(1)(2) = O
    val state = (ZeroSumGame.Max,board)
    
    val action = AlphaBetaSearch(state,game)
    assert(action == (1,0))
  }

  /* Testing the best action at following state,
   * O - O
   * - X -
   * X X O
   *
   * Resulting action should win the game by placing
   * X at (1,2)
   */
  def test3() {
    val board = new Array[Array[Char]](3,3)
    board(2)(2) = O
    board(0)(2) = O
    board(1)(1) = X
    board(0)(0) = X
    board(1)(0) = X
    board(2)(0) = O
    val state = (ZeroSumGame.Max,board)
    
    val action = AlphaBetaSearch(state,game)
    assert(action == (1,2))
  }
}
