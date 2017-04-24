package funprog.recursion.c7

import scala.io.StdIn

/**
  * Functions and Fractals - Recursive Trees
  * https://www.hackerrank.com/challenges/fractal-trees
  *
  * There are 63 rows and 100 columns in the grid below.
  * The triangle is composed of underscores and ones as shown below.
  * The vertical segment and the slanting segments are both 16 characters in length.
  */
object Solution {

  sealed trait YShapes {
    /**
      * Represents this collection of YShapes in a 2-dimensional array.
      * A '1' represents the presence of this shape, the '_' the absence.
      */
    def toLines: Vector[Vector[Char]]
  }
  case object YShapes {
    def apply(size: Int, depth: Int): YShapes = {
      if (depth <= 1) YShape(size)
      else ConnectedShapes(
        head = YShape(size),
        left = apply(size/2, depth-1),
        right = apply(size/2, depth-1))
    }
  }
  case class ConnectedShapes(head: YShape, left: YShapes, right: YShapes) extends YShapes {
    def toLines: Vector[Vector[Char]] = {
      head.toLines ++
        left.toLines.zip(right.toLines)
          .map { case (a,b) => a ++ Vector.fill(head.width/2)('_') ++ b }
    }
  }
  case class YShape(length: Int) extends YShapes {

    val width = length+1 // (length of slanting segment * 2) + 1

    def toLines(): Vector[Vector[Char]] = {
      val centerX = width/2
      val verticalLine = Vector.fill(width)('_').updated(centerX, '1')
      val verticalSegment = (0 until (length / 2)).toVector.map(_ => verticalLine)
      val slantingSegment = (0 until (length / 2)).toVector.map { rowId =>
        Vector.fill(width)('-')
          .updated(centerX - (rowId + 1), '1')
          .updated(centerX + (rowId + 1), '1')
      }
      verticalSegment ++ slantingSegment
    }
  }

  def surroundWith[A](totalSize: Int, element: A)(list: Vector[A]): Vector[A] = {
    val remainder = totalSize - list.size
    val right = remainder/2
    Vector.fill(remainder-right)(element) ++ list ++ Vector.fill(right)(element)
  }

  def main(args: Array[String]): Unit = {
    val depth = StdIn.readInt()
    YShapes(16, depth)
      .toLines
      .map(surroundWith(100, '_'))
      .reverseIterator
      .foreach { line =>
        println(line.mkString)
      }
  }




}
