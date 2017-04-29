package funprog.recursion.c8

import scala.io.StdIn

/**
  * Convex Hull
  * https://www.hackerrank.com/challenges/convex-hull-fp
  *
  * Implemented own algorithm for finding convex hull, which turned out to be similar to quickHull.
  * @author Frank van Meeuwen
  */
object Solution {

  object Point {
    def apply(x: Int, y: Int): Point = IPoint(x,y)
    def apply(x: Double, y: Double): Point = DPoint(x,y)
    def readDoublesFromLine(line: String): Point = {
      val x::y::Nil = line.split(" ").map(_.toDouble).toList
      Point(x,y)
    }
    def readIntsFromLine(line: String): Point = {
      val x::y::Nil = line.split(" ").map(_.toInt).toList
      Point(x,y)
    }
    def distance(a: Point, b: Point): Double = {
      Math.sqrt(Math.pow(b.y - a.y,2) + Math.pow(b.x - a.x,2))
    }
  }
  sealed trait Point {
    def x: Double
    def y: Double
    def distance(other: Point): Double = Point.distance(this,other)
  }
  case class DPoint(x: Double, y: Double) extends Point
  case class IPoint(xInt: Int, yInt: Int) extends Point {
    override val x = xInt.toDouble
    override val y = yInt.toDouble
  }
  case class LineSegment(from: Point, to: Point) {
    lazy val length = from.distance(to)

    def perpendicularDistance(point: Point): Double = {
      val areaTriangle = Math.abs((to.y - from.y) * point.x - (to.x - from.x) * point.y + to.x * from.y - to.y * from.x)
      areaTriangle / length
    }

    def isAbove(point: Point): Boolean = {
      whichSide(point) > 0
    }

    def isBelow(point: Point): Boolean = {
      whichSide(point) < 0
    }

    private def whichSide(point: Point): Double = {
      (to.x - from.x) * (point.y - from.y) - (point.x - from.x) * (to.y - from.y)
    }
  }

  def main(args: Array[String]): Unit = {
    val numberOfPoints = StdIn.readInt
    val points = (0 until numberOfPoints).map(_ => Point.readIntsFromLine(StdIn.readLine)).toList
    val convexHull = calculateConvexHull(points)
    println(convexHull.map(_.length).sum)
  }

  def calculateConvexHull(points: Seq[Point]): List[LineSegment] = {
    val (minX,maxX) = points.tail.foldLeft(points.head -> points.head){ case ((minX,maxX),point) =>
      if(point.x < minX.x) point -> maxX
      else if (point.x > maxX.x) minX -> point
      else minX -> maxX
    }
    val line = LineSegment(minX,maxX)
    expandConvexHull(line, points, aboveLine = true) ++ expandConvexHull(line, points, aboveLine = false)
  }

  /**
    * Given an line that is part of the convex hull and points in the plane.
    * It will split the line in two lines, and tries to expand the convex hull of those lines recursively as well.
    * A line is split if there exists a point above/below the line, it will take the point with the maximum distance perpendicular to the line.
    */
  private def expandConvexHull(line: LineSegment, points: Seq[Point], aboveLine: Boolean): List[LineSegment] = {
    val remainingPoints = if (aboveLine) points.filter(line.isAbove) else points.filter(line.isBelow)

    if (remainingPoints.isEmpty) List(line)
    else {
      val furthestPoint = remainingPoints.maxBy(line.perpendicularDistance)
      val splitLineA = LineSegment(line.from, furthestPoint)
      val splitLineB = LineSegment(furthestPoint, line.to)
      expandConvexHull(splitLineA, remainingPoints, aboveLine) ++ expandConvexHull(splitLineB, remainingPoints, aboveLine)
    }
  }
}
