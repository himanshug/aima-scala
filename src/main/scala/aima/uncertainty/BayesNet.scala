package aima.uncertainty

/** Bayesian Network Representation, as described in Sec 14.1
 *
 * NOTE: Parents(or Causes) MUST be added BEFORE Children(or Effects)
 *
 * @author Himanshu Gupta
 */
class BayesNet {

  private var _variables:List[RandomVariable] = Nil
  private var _nodesMap = Map[RandomVariable,Node]()

  private def findNode(X: RandomVariable) =
    _nodesMap.get(X) match {
      case Some(n) => n
      case None => throw new IllegalArgumentException(X + " has never been added to this bayes network")
    }
  
  //Returns P(X=x|Parents(X)) TODO: change conditions to a Map
  def getProbability(X: RandomVariable, x: String, conditions: Map[RandomVariable,String]): Double = {
    val parents = this.parents(X)
    val e = parents.foldLeft(Map[RandomVariable,String]())(
      (m,v) => m + (v -> conditions(v)))
    findNode(X).cpt.get(e + (X->x)) match {
      case Some(p) => p
      case None =>
        throw new IllegalArgumentException("Could not find posterior probability for " + X + " = " + x + " with conditions " + conditions)
    }
  }

  //Returns probability distribution of X, given that conditions/parents are fixed
  def getProbabilityDistribution(X: RandomVariable, conditions: Map[RandomVariable,String]): Map[String,Double] = {
    val cpt = this.cpt(X)
    val parents = this.parents(X)
    val e = parents.foldLeft(Map[RandomVariable,String]())(
      (m,v) => m + (v -> conditions(v)))
    X.domain.foldLeft(Map[String,Double]())(
      (m,d) => m + (d -> cpt(e + (X->d))))
  }

  //P(X|markovBlanket(X)); Returns probability distribution of X given its markov blanket
  def getMarkovBlanketProbabilityDistribution(X: RandomVariable, mbv: Map[RandomVariable,String]): Map[String,Double] = {
    //algo is described in section-14.5.2, last paragraph
    // P(xi|mb(Xi)) = alpha*P(xi|parents(Xi)) * P(y1|parents(Y1)) * P(y2|parents(Y2))...
    //     where yj are children of Xi
    X.domain.foldLeft(Map[String,Double]())(
      (m,d) => {
        val p = getProbability(X,d,mbv) * 
          children(X).foldLeft(1.0)(
            (p,y) => p * getProbability(y,mbv(y),mbv + (X -> d)))
        m + (d -> p)
      })
  }

  //Returns CPT of a node
  def cpt(X: RandomVariable) = findNode(X).cpt
 
  def children(X: RandomVariable): Set[RandomVariable] = findNode(X).children.map(_.variable)
  def parents(X: RandomVariable): Set[RandomVariable] = findNode(X).parents.map(_.variable)

  //topologically sorted list of variables
  def variables = _variables

  def add(variable: RandomVariable, parents: Set[RandomVariable], cpt: Map[Map[RandomVariable,String],Double]): BayesNet = {
    //make sure this variable is not already added
    if(!_nodesMap.contains(variable)) {
      val node = new Node(variable,parents.map(findNode(_)),cpt)
      _nodesMap = _nodesMap + (variable -> node)
      _variables = _variables ::: List(variable)
      this
    }
    else  throw new RuntimeException(variable + " has already been added to this Bayes net")
  }
}

//Representation for a node in the Bayes Network
class Node(val variable: RandomVariable, val parents: Set[Node],
           val cpt: Map[Map[RandomVariable, String],Double]) {

  //for all parents, add this node as child node
  parents.foreach(_.addChild(this))

  private var _children = Set[Node]()

  def children = _children

  def addChild(node: Node) { _children = _children + node }
}
