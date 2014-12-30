package aima.commons

object Utils {

  /**
   * Returns (N combinatorial 2) pairs of the elements
   * in the given list, N = list.length
   * Returns Nil: If N < 2
   */
  //TODO: Can we make it lazy somehow
  def pairs[A](list: List[A]): List[(A,A)] = {

    def oLoop(list: List[A], result: List[(A,A)]): List[(A,A)] =
      list match {
        case x :: xs => oLoop(xs, iLoop(x,xs,Nil) ++ result)
        case Nil => result
      }

    def iLoop(elm: A, list: List[A], result: List[(A,A)]): List[(A,A)] =
      list match {
        case x :: xs => iLoop(elm, xs, (elm,x) :: result)
        case Nil => result
      }

    oLoop(list,Nil)
  }

  /**
   * Returns all the subsets of a given set.
   */
  def subsets[A](ss: Set[A]): Set[Set[A]] = {
    def loop(ls: List[A], result: Set[Set[A]]): Set[Set[A]] =
      ls match {
        case x :: rest =>
          loop(rest,result ++ result.map(_ + x))
        case Nil => result
      }
    loop(ss.toList,Set(Set()))
  }

  /**
   * Returns all pairs from elements in "items" that satisfy
   * the give "pred" condition
   */
  def pairs[A](items: Set[A], pred: (A,A)=>Boolean): Set[(A,A)] = {
    
    def loop(items: List[A], result: Set[(A,A)]): Set[(A,A)] =
      items match {
        case x :: rest =>
          var tmp = Set[(A,A)]()
          for(y <- rest) {
            if(pred(x,y)) tmp = tmp + ((x,y))
          }
          loop(rest,result ++ tmp)
        case Nil => result
      }

    loop(items.toList,Set.empty)
  }
}
