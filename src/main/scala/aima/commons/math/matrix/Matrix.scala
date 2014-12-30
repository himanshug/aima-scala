package aima.commons.math.matrix

class Matrix[T: ClassManifest] private (private val elems: Array[Array[T]])(n: Numeric[T]) {

  import n.mkNumericOps

  val rows = elems.size        //Returns # of rows
  val cols = elems(0).size     //Returns # of cols

  //Returns jth elem in ith row
  // 0 <= i < rows And 0 <= j < cols
  def apply(i: Int, j: Int): T = elems(i)(j)

  //Returns ith row, 0 <= i < rows
  def row(i: Int): Array[T] = elems(i).clone

  //Returns ith col, 0 <= i < cols
  def col(i: Int): Array[T] = elems.map(_(i))

  def *(that: Matrix[T]): Matrix[T] = {
    if(this.cols != that.rows)
      throw new IllegalArgumentException("Can't be multiplied")
    else {
      val newArr = Array.ofDim[T](this.rows, that.cols)

      for(i <- 0 to this.rows-1) {
        for(j <- 0 to that.cols-1) {
          newArr(i)(j) = this.row(i).zip(that.col(j)).map(a => a._1 * a._2).sum(n)
        }
      }
      new Matrix(newArr)(n)
    }
  }
}

object Matrix {

  def apply[T](elems: Array[Array[T]])(implicit n: Numeric[T],m: ClassManifest[T]): Matrix[T] =
    new Matrix[T](clone(elems))(n)

  private def clone[T: ClassManifest](arr: Array[Array[T]]): Array[Array[T]] = arr.map(_.clone)
}
