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

  val Present = '1'
  val NotPresent = '_'

  sealed trait YShapesList {
    /**
      * Represents this collection of YShapes in a 2-dimensional array.
      * A Present represents the presence of this shape, the '_' the absence.
      */
    def toLines: Vector[Vector[Char]]
  }
  case object Empty extends YShapesList {
    override def toLines: Vector[Vector[Char]] = Vector.empty
  }
  case object YShapes {
    def apply(size: Int, depth: Int): YShapes = {
      if (depth <= 1) YShapes(YShape(size), Empty, Empty)
      else YShapes(YShape(size), apply(size/2, depth-1), apply(size/2, depth-1))
    }
  }
  case class YShapes(head: YShape, left: YShapesList, right: YShapesList) extends YShapesList {
    def toLines: Vector[Vector[Char]] = {
      head.toLines(head.width*2-2) ++
        left.toLines.zip(right.toLines)
          .map { case (a,b) => a ++ b }
    }
  }
  case class YShape(length: Int) {

    val width = length+1 // (length of slanting segment * 2) + 1

    def toLines(totalSize: Int): Vector[Vector[Char]] = {
      val centerX = width/2
      val verticalLine = Vector.fill(width)(NotPresent).updated(centerX, Present)
      val verticalSegment = (0 until (length / 2)).toVector.map(_ => verticalLine)
      val slantingSegment = (0 until (length / 2)).toVector.map { rowId =>
        Vector.fill(width)(NotPresent)
          .updated(centerX - (rowId + 1), Present)
          .updated(centerX + (rowId + 1), Present)
      }
      (verticalSegment ++ slantingSegment)
        .map(surroundWith(totalSize,NotPresent))
    }
  }

  def surroundWith[A](totalSize: Int, element: A)(list: Vector[A]): Vector[A] = {
    val remainder = totalSize - list.size
    val left = remainder/2
    Vector.fill(left)(element) ++ list ++ Vector.fill(remainder-left)(element)
  }

  def ensureSize[A](totalSize: Int, fill: A)(list: Vector[A]): Vector[A] = {
    val firstList = list.take(totalSize)
    firstList ++ Vector.fill(totalSize - firstList.size)(fill)
  }

  def main(args: Array[String]): Unit = {
    val depth = StdIn.readInt()
    val lines: Vector[Vector[Char]] = YShapes(32, depth)
      .toLines
      .map(surroundWith(100, NotPresent))

    ensureSize(63, Vector.fill(100)(NotPresent))(lines)
      .reverseIterator
      .foreach { line =>
        println(line.mkString)
      }
  }




}
