package aima.search

/** Incremental Formulation of NQueensProblem as described in AIMA
 *
 * @author Himanshu Gupta
 */
class NQueensProblem(size: Int) extends Problem[NQueensState,Put] {

  override def initialState = NQueensState(size)

  override def goalTest(s: NQueensState) = (s.numQueens == size)

  override def actions(s: NQueensState): List[Put] = {

    def loop(i:Int, resultActions: List[Put]): List[Put] = {
      if (i > size) resultActions
      else {
        if (s.isSafe(Put(i)))
          loop(i+1, Put(i) :: resultActions)
        else loop(i+1, resultActions)
      }
    }
    loop(1,Nil)
  }

  override def result(s: NQueensState, a: Put): NQueensState = s.executeAction(a)

  //TODO to be implemented properly
  override def stepCost(from: NQueensState, action: Put, to: NQueensState): Double = 0.0

  //TODO to be implemented properly
  override def estimatedCostToGoal(from: NQueensState): Double = 0.0
}

//"Action" for the NQueensProblem
case class Put(y: Int)

/* "State" for NQueensProblem
 * 
 * NQueensState represents state of the board containing 0 or more queens
 * on the board.
 * 
 * size: Size of the board
 * co-ordinates x,y go from 1 to size, both start at bottom left corner of the board
 * 
 * queens: current state of queens on the board, queens = List(5,4,3) means
 * 3 queens at following positions
 * X: 3 2 1
 * Y: 5 4 3
 *  
 * */
class NQueensState private (size: Int, queens: List[Int]) {

  def numQueens = queens.length

  //add a queen in next available column
  def executeAction(a: Put) = {
    if (numQueens == size)
      throw new IllegalStateException("Can't place one more queen. Board is full.")
    else
      a match {
        case Put(y) => new NQueensState(size, y :: queens)
      }
  }

  // Checks if the action is safe
  def isSafe(action: Put): Boolean = {
    //check if two positions are attacking each other
    def isAttacking(x1: Int, y1: Int, x2: Int, y2: Int) =
      !(x1 == x2 && y1 == y2) && ( x1 == x2 || y1 == y2 || Math.abs(x1-x2) == Math.abs(y1-y2))

    val len = numQueens
    val newX = len + 1
    val Put(newY) = action

    def loop(me: List[Int], x: Int): Boolean =
      me match {
        case Nil => true
        case y :: rest if isAttacking(x,y,newX,newY) => false
        case _ :: rest => loop(rest, x-1)
      }

    loop(queens,len)
  }

  //printable board representation
  override def toString() = {
    var result = ""
    val tmp = queens.reverse
    val len = numQueens

    for(y <- 1 to size) {
      for(x <- 1 to size) {
        if(x <= len && tmp(x-1) == (size+1-y))
          result = result + "X "
        else
          result = result + "- "
      }
      result = result + "\n"
    }
    result
  } 
}
object NQueensState {
  def apply(size: Int) = new NQueensState(size,List())
}


