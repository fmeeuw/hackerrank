package funprog.recursion.c4

/**
  * Functions and Fractals: Sierpinski triangles
  * https://www.hackerrank.com/challenges/functions-and-fractals-sierpinski-triangles
  *
  *
  */
 object Solution {

  trait Triangle {
    def baseWidth: Int
    def split: Triangle
    def splitRecursively(depth: Int): Triangle = {
      if (depth <= 0) this
      else split.splitRecursively(depth-1)
    }
    /**
      * Represents this triangle as a 2-dimensional vector of chars, where '1' denotes the presence of this triangle and '_' its absence.
      * Starts at the top of the triangle.
      */
    def toGrid: Vector[Vector[Char]]
  }
  case class BaseTriangle(override val baseWidth: Int) extends Triangle {

    override def split: Triangle = ComposedTriangle(
      BaseTriangle(baseWidth/2),
      BaseTriangle(baseWidth/2),
      BaseTriangle(baseWidth/2)
    )

    override def toGrid: Vector[Vector[Char]] = {
      def rowWidth(rowId: Int): Int = rowId*2+1
      def rows: Int = (baseWidth+1)/2

      (0 until rows).map { rowId =>
        val width = rowWidth(rowId)
        val fillSide = Vector.fill((baseWidth-width)/2)('_')
        fillSide ++ Vector.fill(width)('1') ++ fillSide
      }.toVector
    }
  }
  case class ComposedTriangle(top: Triangle, left: Triangle, right: Triangle) extends Triangle {
    override def baseWidth: Int = left.baseWidth + right.baseWidth + 1

    override def split: Triangle = ComposedTriangle(top = top.split, left = left.split, right = right.split)

    override def toGrid: Vector[Vector[Char]] = {
      val fillTop = Vector.fill((baseWidth - top.baseWidth) / 2)('_')
      top.toGrid.map { mid => fillTop ++ mid ++ fillTop } ++
        left.toGrid.zip(right.toGrid).map { case (l,r) => l ++ Vector('_') ++ r }
    }
  }

  def drawTriangles(n: Int) {
    BaseTriangle(63)
      .splitRecursively(n)
      .toGrid
      .foreach{ row => println(row.mkString)}
  }

  def main(args: Array[String]) {
    drawTriangles(readInt())
  }

}
