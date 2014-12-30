package aima.search.csp 

// Algorithms to Solve Constraint Satisfaction Problems with Binary Constraints



/** Implementation of AC-3 and MAC (Maintaining Arc Consistency)
 * inferencing algorithms
 * @author Himanshu Gupta
 */
object Inference {

  //************************ ARC ************************
  //
  //One binary constraint leads to two arcs(or variable pairs)
  //in the queue, we also keep the constraint between those
  //variables in the queue, so queue is a list of triplets
  //For example a binary constraint, c with 2 variables x1
  //and x2 results in two triplets in the queue
  //(x1,x2,c) and (x2,x1,c)


  /** AC-3 algorithm, described in Fig 6.3
   * 
   * A slight variation is that instead of returning
   * true/false it returns a CSP with inconsistent
   * values removed from domain of various variables
   * or None if domain of one of the variables becomes
   * empty.
   *
   * @author Himanshu Gupta
   */
  def AC3[K,V](csp: CSP[K,V]): Option[CSP[K,V]] = {

    //Prepare the queue containing all the arcs
    var queue = List[(K,K,Constraint[K,V])]()
    csp.constraints.foreach(c => {
                              val (x,y) = c.variables
                              queue = (x,y,c) :: (y,x,c) :: queue })

    makeArcConsistent(csp,queue)
  }

  /*** MAC ( Maintaining Arc Consistency) Inferencing Algorithm, described
   * in section 6.3.2
   *
   * @author Himanshu Gupta
   */
  def MAC[K,V](x : K, assignment: Map[K,V], csp: CSP[K,V]): Option[CSP[K,V]] = {
    //Prepare the queue with all the neighbours of x that are
    //unassigned yet, pass it to makeArcConsistent
    def loop(neighbours: List[(K,Constraint[K,V])], queue: List[(K,K,Constraint[K,V])]): List[(K,K,Constraint[K,V])] =
      neighbours match {
        case (xj,c) :: rest if !(assignment.contains(xj)) =>
                                      loop(rest, (xj,x,c) :: queue)
        case _ :: rest => loop(rest, queue)
        case Nil => queue
      }

    makeArcConsistent(csp,loop(csp.neighbours(x),Nil))
  }

  //Returns None if current input assignment is not possible
  //Or else returns a new CSP with inconsistent values of various
  //variables from their domain removed
  private def makeArcConsistent[K,V](csp: CSP[K,V], queue: List[(K,K,Constraint[K,V])]): Option[CSP[K,V]] = {
      queue match {
        case (x,y,c) :: rest => {
          revise(csp,x,y,c) match {
            case None =>  //No change to the domain of x
              makeArcConsistent(csp,rest)
            case Some(domainX) => //domain of x reduced
              if (domainX.isEmpty) None
              else {
                val neighbours = csp.neighbours(x) //returns a list of pair (neighbour,constraint)
                var newQ = rest
                neighbours.foreach(nbr => if(nbr._1 != y) newQ = (nbr._1,x,nbr._2) :: newQ)
                makeArcConsistent(csp.clone(x -> domainX),newQ)
              }
          }
        }
        case Nil => Some(csp)
      }
  }

  //Returns new domain of x if changed or None
  private def revise[K,V](csp: CSP[K,V], x: K, y: K, constraint: Constraint[K,V]): Option[List[V]] = {
    val domainXi = csp.domain(x)
    val domainYi = csp.domain(y)
    val consistentDomainXi = csp.domain(x).filter( xi =>
                                        domainYi.exists( yi => constraint.isSatisfied(Map(x -> xi, y -> yi))))
    if(consistentDomainXi.length != domainXi.length)
      Some(consistentDomainXi)
    else None
  }
}

/** Backtracking-Search, described in Fig 6.5
 *
 * @author Himanshu Gupta
 */
object BacktrackingSearch {
  //BacktrackingSearch without any inferencing
  def apply[K,V](csp: CSP[K,V]) =
    backtrack(Map[K,V](),csp,identityInference[K,V])


  //BacktrackingSearch with given inference algorithm
  def apply[K,V](csp: CSP[K,V],inference: (K,Map[K,V],CSP[K,V])=>Option[CSP[K,V]]) =
    backtrack(Map[K,V](),csp,inference)

  def backtrack[K,V](assignment: Map[K,V],csp: CSP[K,V], inference: (K,Map[K,V],CSP[K,V])=>Option[CSP[K,V]]): Option[Map[K,V]] = {
    if (csp.isComplete(assignment)) Some(assignment)
    else {
      var variable = selectUnassignedVariable(csp.variables,assignment,csp) match {
        case Some(s) => s
        case None => throw new IllegalStateException("No unassigned variable found.")}

      val values = orderDomainValues(variable,assignment,csp)
      
      def loop(values: List[V]): Option[Map[K,V]] =
        values match {
          case value :: rest => {
            val newAssignment = assignment + (variable -> value)
            if (csp.isAssignmentOk(newAssignment)) {
              inference(variable,newAssignment,csp) match {
                case None => loop(rest)
                case Some(newCsp) =>
                  val result = backtrack(newAssignment,newCsp,inference)
                  if (result != None) result else loop(rest)
              }
            }
            else loop(rest)
          }
          case Nil => None
        }
      loop(values)
    }
  }

  private def selectUnassignedVariable[K,V](variables: List[K],assignment: Map[K,V],csp: CSP[K,V]) =
    variables.find(!assignment.contains(_))

  private def orderDomainValues[K,V](variable: K, assignment: Map[K,V], csp: CSP[K,V]) =
    csp.domain(variable)

  //An inference that does no inferencing
  private def identityInference[K,V](x: K, assignment: Map[K,V], csp: CSP[K,V]): Option[CSP[K,V]] = Some(csp)
}


/** Min-Conflicts, described in Fig 6.8
 *
 * @author Himanshu Gupta
 */
object MinConflicts {
  def apply[K,V](csp: CSP[K,V],maxSteps: Int) = {
    
    def loop(current: Map[K,V],count: Int): Option[Map[K,V]] = {
      if(count < maxSteps) {
        if (csp.isComplete(current)) Some(current)
        else {
          var variable = randomlyChoseVariableInConflict(csp,current)
          val value = valueThatMinimizesConflicts(variable,current,csp)
          loop(current + (variable -> value),count+1)
        }
      }
      else None }

    loop(randomFullAssignment(csp),0)
  }

  //Returns a randomly generated full assignment that may or
  //may not satisfy all the constraints
  private def randomFullAssignment[K,V](csp: CSP[K,V]) = {
    
    val random = new scala.util.Random(new java.util.Random)
    def randomValue(variable: K) = {
      val domain = csp.domain(variable)
      if(domain.length > 0)
        domain(random.nextInt(domain.length))
      else
        throw new IllegalStateException("domain for " + variable + " is empty.")
    }

    Map[K,V]() ++ (csp.variables.map((x) => (x, randomValue(x))))
  }

  private def valueThatMinimizesConflicts[K,V](variable: K, assignment: Map[K,V], csp: CSP[K,V]) = {
    val domain = csp.domain(variable)
    domain.foldLeft(domain.head)( (p,q) => if (csp.constraintsInConflict(assignment + (variable -> p)).length <
                                                                  csp.constraintsInConflict(assignment + (variable -> q)).length)
                                                            p
                                                            else q )
  }

  //returns a variable(randomly chosen,higher probability for higher number of conflicts) in conflict, 
  //provided given assignment is not partial and atleast one constraint is broken
  private def randomlyChoseVariableInConflict[K,V](csp: CSP[K,V], assignment: Map[K,V]) = {
    val random = new scala.util.Random(new java.util.Random)
    def loop(constraints: List[Constraint[K,V]], variablesInConflict: List[K]): List[K] =
      constraints match {
        case c :: rest => {
          if (!c.isSatisfied(assignment)) {
            val (x,y) = c.variables
            (assignment.contains(x),assignment.contains(y)) match {
              case (true,true) => loop(rest, x :: y :: variablesInConflict)
              case (true,false) => loop(rest,x :: variablesInConflict)
              case (false,true) => loop(rest, y :: variablesInConflict)
              case (false,false) => loop(rest, variablesInConflict)
            }
          }
          else loop(rest, variablesInConflict) }
        case Nil => variablesInConflict
      }
    loop(csp.constraints,Nil) match {
      case Nil => throw new IllegalStateException("assignment is either complete or partial.")
      case x => x(random.nextInt(x.length)) }
    
  }
}

/** Tree-CSP-Solver, described in Fig 6.11
 *
 * @author Himanshu Gupta
 */
object TreeCspSolver {

  def apply[K,V](csp: TreeCSP[K,V]): Option[Map[K,V]] = {
    
    val variables = csp.variables //topologically sorted

    def loop(vars: List[Node[K,V]], csp: TreeCSP[K,V]): Option[TreeCSP[K,V]] =
      vars match {
        case _ :: Nil => Some(csp)
        case x :: rest =>
          makeArcConsistent(csp.parent(x),x,csp) match {
            case None => None
            case Some(newCsp) => loop(rest,newCsp)
          }
        case Nil => throw new IllegalStateException("Tree CSP with No variables.")
      }

    loop(variables.reverse,csp) match {
      case None => None
      case Some(csp) =>
        def anotherLoop(nodes: List[Node[K,V]], result: Map[K,V]): Option[Map[K,V]] =
          nodes match {
            case n :: rest =>
              csp.domain(n).find( (v:V) =>
                csp.constraint(csp.parent(n),n).isSatisfied(result + (n.key -> v))) match {
                case Some(v) => anotherLoop(rest,result + (n.key -> v))
                case None => None
              }
            case Nil => Some(result)
          }
        anotherLoop(variables.tail,Map(variables.head.key -> csp.domain(variables.head).head))
    }
  }

  private def makeArcConsistent[K,V](parent: Node[K,V], child: Node[K,V], csp: TreeCSP[K,V]): Option[TreeCSP[K,V]] = {
    val domainXi = csp.domain(parent)
    val domainYi = csp.domain(child)
    val constraint = csp.constraint(parent,child)
    val consistentDomainXi = domainXi.filter( xi =>
                                        domainYi.exists( yi => constraint.isSatisfied(Map(parent.key -> xi, child.key -> yi))))
    if(consistentDomainXi.isEmpty) None
    else Some(csp.setDomain(child, consistentDomainXi))
  }
}
