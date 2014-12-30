package aima.search.csp

/** Abstract Representation for Tree CSP
 *
 * @author Himanshu Gupta
 */
class TreeCSP[K,V](x: K, domain: List[V]) {

  val root = new Node(x,domain,None,None)

  //Returns topologically sorted list of nodes
  def variables: List[Node[K,V]] = {

    def loop(node: Node[K,V]): List[Node[K,V]] = {
      if(node.isLeaf)
        List(node)
      else
        node :: node.children.flatMap(loop(_))
    }
    loop(root)
  }

  def parent(node: Node[K,V]) =
    node.parent match {
      case Some(p) => p
      case None =>
        throw new IllegalStateException("give node has no parents")
    }

  def domain(node: Node[K,V]) =
    node.domain

  def setDomain(node: Node[K,V], domain: List[V]) = {
    node.domain = domain
    this
  }

  def constraint(parent: Node[K,V], child: Node[K,V]) =
    child.constraint match {
      case Some(c) => c
      case None => throw new IllegalStateException("no constraint found for " + parent.key + " and " + child.key)
    }

  def addChild(parent: Node[K,V], childKey: K, domain: List[V], constraint: Constraint[K,V]) =
    parent.addChild(new Node(childKey,domain,Some(parent),Some(constraint)))
}


/** Tree CSP Node */
class Node[K,V](val key: K, var domain: List[V], val parent: Option[Node[K,V]],
                val constraint: Option[Constraint[K,V]]) {

  private var _children = List[Node[K,V]]()

  def addChild(child: Node[K,V]) = {
    _children = child :: _children
    child
  }

  def children = _children

  def isLeaf = _children.isEmpty
}
