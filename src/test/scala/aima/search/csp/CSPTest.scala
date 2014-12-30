package aima.search.csp

import junit.framework._
import Assert._

class AC3Test extends TestCase {

  /* This tests the AC3 algorithm with problem described
   * in section 6.2.2
   *
   * There are two variables x, y and constraint is y = x*x
   * we begin with all 0-9 in the domain of both the variables.
   * After AC3 is applied, domain of x should become {0,1,2,3}
   * and that of y should become {0,1,4,9}
   */
  def testAC3() {
    val X = "X"
    val Y = "Y"
    val constraint = new Constraint[String,Int](X,Y) {
      def isSatisfied(assignment: Map[String,Int]) =
        (assignment.get(X),assignment.get(Y)) match {
          case (Some(valX),Some(valY)) => valY == valX*valX
          case _ => false
        }
    }

    val domain = List(0,1,2,3,4,5,6,7,8,9)

    val csp = new CSP[String,Int]()
    csp.addConstraints(constraint)
    csp.addVariables((X,domain),(Y,domain))

    Inference.AC3(csp) match {
      case None => assert(false)
      case Some(csp) =>
        val domainX = csp.domain(X)
        val domainY = csp.domain(Y)

        assertEquals(domainX.length,4)
        assertEquals(domainY.length,4)
        
        val testDomainX = List(0,1,2,3)
        val testDomainY = List(0,1,4,9)

        assertTrue(domainX.forall( x => testDomainX.exists(_ == x)))
        assertTrue(domainY.forall( y => testDomainY.exists(_ == y)))
    }
  }
}

class BacktrackingSearchTest extends TestCase {

  /*******************************************************/
  /**** Backtracking-Search Tests with NO Inferencing ****/
  /*******************************************************/

  def testBacktrackingSearchForAustraliaMapColorCSP() {
    import AustraliaMapColorCSP._

    BacktrackingSearch(csp) match {
      case None => assert(false)
      case Some(assignment) => {
        assertEquals(assignment.getOrElse(Wa,-1),Red)
        assertEquals(assignment.getOrElse(Nt,-1),Blue)
        assertEquals(assignment.getOrElse(Q,-1),Red)
        assertEquals(assignment.getOrElse(Sa,-1),Green)
        assertEquals(assignment.getOrElse(Nsw,-1),Blue)
        assertEquals(assignment.getOrElse(V,-1),Red)
        assertTrue(assignment.getOrElse(T,-1) == Red ||
               assignment.getOrElse(T,-1) == Blue ||
               assignment.getOrElse(T,-1) == Green)
      }
    }
  }

  def testBacktrackingSearchForNQueensCSP() {
    
    val csp = NQueensCSP.csp(8)
    BacktrackingSearch(csp) match {
      case None => assertTrue(false)
      case Some(assignment) =>
        //println("Nqueeens solution found: " + csp.toString(assignment))        
        assertTrue(true)
    }
  }

  /*******************************************************/
  /**** Backtracking-Search Tests with MAC Inferencing ***/
  /*******************************************************/

  def testBacktrackingSearchWithMACForAustraliaMapColorCSP() {
    import AustraliaMapColorCSP._

    //Note: Somehow, Inference.MAC is not transparently convering
    //into a Function value, so I'm doing it manually.
    //TODO: This needs to be investigated
    //Ideally we should be able to call BacktrackingSearch(csp,Inference.MAC)
    //directly
    val mac = (x: String, a: Map[String,Int], csp: CSP[String,Int]) => {
      Inference.MAC(x,a,csp) }

    BacktrackingSearch(csp,mac) match {
      case None => assert(false)
      case Some(assignment) => {
        assertEquals(assignment.getOrElse(Wa,-1),Red)
        assertEquals(assignment.getOrElse(Nt,-1),Blue)
        assertEquals(assignment.getOrElse(Q,-1),Red)
        assertEquals(assignment.getOrElse(Sa,-1),Green)
        assertEquals(assignment.getOrElse(Nsw,-1),Blue)
        assertEquals(assignment.getOrElse(V,-1),Red)
        assertTrue(assignment.getOrElse(T,-1) == Red ||
               assignment.getOrElse(T,-1) == Blue ||
               assignment.getOrElse(T,-1) == Green)
      }
    }
  }
}

class MinConflictsTest extends TestCase {
  
  def testIt() {
    MinConflicts(AustraliaMapColorCSP.csp,100) 
  }
}

class TreeCspSolverTest extends TestCase {

  /** Test the tree csp described in Fig 6.10
   * A
   * |
   * B
   * |\
   * C D
   *   |\
   *   E F
   *
   * Problem is to assign Red/Blue color to each variable
   * so that no two connected ones get the same color.
   */
  def testIt() {
    val Red = 0
    val Blue = 1
    val domain = List(Red,Blue)
    
    val csp = new TreeCSP[String,Int]("A",domain)
    
    //add constraints
    val b = csp.addChild(csp.root, "B", domain, new AllDiffConstraint[String,Int]("A","B"))
    csp.addChild(b, "C", domain, new AllDiffConstraint[String,Int]("B","C"))

    val d = csp.addChild(b, "D", domain, new AllDiffConstraint[String,Int]("B","D"))
    csp.addChild(d, "E", domain, new AllDiffConstraint[String,Int]("D","E"))
    csp.addChild(d, "F", domain, new AllDiffConstraint[String,Int]("D","F"))

    TreeCspSolver(csp) match {
      case None => assertTrue(false)
      case Some(sol) =>
        assertTrue((sol("A") == Red && sol("B") == Blue && sol("C") == Red && sol("D") == Red && sol("E") == Blue && sol("F") == Blue) ||
               (sol("A") == Blue && sol("B") == Red && sol("C") == Blue && sol("D") == Blue && sol("E") == Red && sol("F") == Red))
    }
  }
}
                         
