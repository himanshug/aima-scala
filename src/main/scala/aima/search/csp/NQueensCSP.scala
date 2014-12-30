package aima.search.csp

/** Factory for N-Queens CSP
 *
 * @author Himanshu Gupta
 */
object NQueensCSP {
  
  private def generateDomain(n: Int,result: List[Int]): List[Int] = 
    if(n == 0) result else generateDomain(n-1,n :: result)
  
  def csp(n: Int) = {
    val csp = new CSP[Queen,Int]() {
      //overriding toString to print a human-friendly version of
      //the assignment
      override def toString(assignment: Map[Queen,Int]) = {
        val n = assignment.size
        var result = "\n"
        for(y <- n until (0,-1)) {
          for(x <- 1 to n) {
            assignment.get(Queen(x)) match {
              case None => throw new IllegalStateException(Queen(x) + " is not assigned.")
              case Some(yPos) => 
                result = result + (if(y == yPos) "X " else "- ")
            }
          }
          result = result + "\n"
        }
        result
      }
    }

    val domain = generateDomain(n,Nil)
    
    //add n Queens
    for(i <- 1 to n) { csp.addVariables((Queen(i),domain)) }

    //add constraint for every pair of queen
    for(i <- 1 to n; j <- i+1 to n) { csp.addConstraints(new NQueensConstraint(Queen(i),Queen(j))) }

    csp //return the csp
  }
}


case class Queen(n: Int) //NQueens CSP variable type

//NQueens CSP constraint type
class NQueensConstraint(x1: Queen, x2: Queen) extends Constraint[Queen,Int](x1,x2) {

  //check that no two queens are not atacking each other
  //in the given assignment
  def isSatisfied(assignment: Map[Queen,Int]) =
    (assignment.get(x1),assignment.get(x2)) match {
      case(None,_) => false
      case (_, None) => false
      case (Some(qy1),Some(qy2)) if qy1 == qy2 => false
      case (Some(qy1),Some(qy2)) => {
        (x1,x2) match {
          case (Queen(qx1),Queen(qx2)) => Math.abs(qy1 - qy2) != Math.abs(qx1 - qx2) }
      }
    }
}
