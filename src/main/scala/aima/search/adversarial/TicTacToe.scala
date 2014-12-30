package aima.search.adversarial

/** A TicTacToe implementation of ZeroSumGame
 *
 * @author Himanshu Gupta
 */
class TicTacToeGame extends ZeroSumGame[(String,Array[Array[Char]]),(Int,Int)] {

  type Board = Array[Array[Char]]
  type State = (String,Board)
  type Action = (Int,Int)

  private val X = 'X'
  private val O = 'O'
  private val nullChar: Char = 0

  def initialState = (ZeroSumGame.Max,new Array[Array[Char]](3,3))

  def player(s: State) = s._1

  def actions(s: State) = {
    (for(x <- 0 to 2;
         y <- 0 to 2;
         if s._2(x)(y) == nullChar) yield (x,y)).toList
  }

  def result(s: State, a: Action) =
    s match {
      case (ZeroSumGame.Max, board) =>
        if(board(a._1)(a._2) == nullChar) {
          val newBoard = cloneBoard(board)
          newBoard(a._1)(a._2) = X
          (ZeroSumGame.Min, newBoard)
        }
        else throw new IllegalStateException("Box at " + a + " is already filled.")
      case (ZeroSumGame.Min, board) =>
        if(board(a._1)(a._2) == nullChar) {
          val newBoard = cloneBoard(board)
          newBoard(a._1)(a._2) = O
          (ZeroSumGame.Max, newBoard)
        }
        else throw new IllegalStateException("Box at " + a + " is already filled.")
      case _ => throw new IllegalStateException("Not a valid player " + s._1)
    }

  def terminalTest(s: State) =
    getWinner(s._2) match {
      case Some(_) => true
      case None => actions(s).length == 0
    }

  def utility(s: State): Double =
    getWinner(s._2) match {
      case Some(ZeroSumGame.Max) => 1.0
      case Some(ZeroSumGame.Min) => -1.0
      case None if actions(s).length == 0 => 0.5
      case _ => throw new IllegalStateException("Not a terminal state." + toString(s))
    }
      
  def toString(state: State) = {
    val board = state._2
    var result = ""
    for(y <- 2.until(-1,-1); x <- 0 to 2) {
      result = result + (if(board(x)(y) == nullChar) "-" else board(x)(y))
      if(x == 2) result = result + "\n"
    }
    result
  }

  //Returns the winner if game has terminated without draw,
  //None otherwise
  private def getWinner(board: Board): Option[String] = {
    (for( x <- 0 to 2; y <- 0 to 2) yield board(x)(y)).toList match {
      //the for loop results in board charaters at all the
      //co-ordinates in following order
      //((0,0), (0,1), (0,2), (1,0), (1,1), (1,2), (2,0), (2,1), (2,2))

      case X :: X :: X :: _ :: _ :: _ :: _ :: _ :: _ :: Nil => Some(ZeroSumGame.Max)
      case _ :: _ :: _ :: X :: X :: X :: _ :: _ :: _ :: Nil => Some(ZeroSumGame.Max)
      case _ :: _ :: _ :: _ :: _ :: _ :: X :: X :: X :: Nil => Some(ZeroSumGame.Max)
      case X :: _ :: _ :: X :: _ :: _ :: X :: _ :: _ :: Nil => Some(ZeroSumGame.Max)
      case _ :: X :: _ :: _ :: X :: _ :: _ :: X :: _ :: Nil => Some(ZeroSumGame.Max)
      case _ :: _ :: X :: _ :: _ :: X :: _ :: _ :: X :: Nil => Some(ZeroSumGame.Max)
      case X :: _ :: _ :: _ :: X :: _ :: _ :: _ :: X :: Nil => Some(ZeroSumGame.Max)
      case _ :: _ :: X :: _ :: X :: _ :: X :: _ :: _ :: Nil => Some(ZeroSumGame.Max)

      case O :: O :: O :: _ :: _ :: _ :: _ :: _ :: _ :: Nil => Some(ZeroSumGame.Min)
      case _ :: _ :: _ :: O :: O :: O :: _ :: _ :: _ :: Nil => Some(ZeroSumGame.Min)
      case _ :: _ :: _ :: _ :: _ :: _ :: O :: O :: O :: Nil => Some(ZeroSumGame.Min)
      case O :: _ :: _ :: O :: _ :: _ :: O :: _ :: _ :: Nil => Some(ZeroSumGame.Min)
      case _ :: O :: _ :: _ :: O :: _ :: _ :: O :: _ :: Nil => Some(ZeroSumGame.Min)
      case _ :: _ :: O :: _ :: _ :: O :: _ :: _ :: O :: Nil => Some(ZeroSumGame.Min)
      case O :: _ :: _ :: _ :: O :: _ :: _ :: _ :: O :: Nil => Some(ZeroSumGame.Min)
      case _ :: _ :: O :: _ :: O :: _ :: O :: _ :: _ :: Nil => Some(ZeroSumGame.Min)

      case _ => None
    }
  }
 
  private def cloneBoard(board: Board): Board = {
    val result = new Array[Array[Char]](3,3)
    for(x <- 0 to 2; y <- 0 to 2) {
      result(x)(y) = board(x)(y)
    }
    result
  }
}
