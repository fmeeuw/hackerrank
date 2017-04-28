package funprog.recursion.c8

import scala.io.StdIn

/**
  * Convex Hull
  * https://www.hackerrank.com/challenges/convex-hull-fp
  *
  * Implemented own algorithm for finding convex hull.
  *
  * @author Frank van Meeuwen
  */
object Solution {

  object Point {
    def apply(x: Int, y: Int): Point = IPoint(x,y)
    def apply(x: Double, y: Double): Point = DPoint(x,y)
    def readIntsFromLine(line: String): Point = {
      val x::y::Nil = line.split(" ").map(_.toInt).toList
      IPoint(x,y)
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
    def toLine: Line = {
      val coefficient = (to.y - from.y) / (to.x - from.x)
      val offset = from.y - (coefficient * from.x)
      Line(offset, coefficient)
    }
    lazy val length: Double = from.distance(to)
    lazy val center: Point = Point(to.x - from.x, to.y - from.y)
  }
  case class Line(offset: Double, coefficient: Double) {
    def getY(x: Double): Double = offset + (x * coefficient)
    def getX(y: Double): Double = (y - offset) / coefficient

    def perpendicularDistance(point: Point): Double = {
      val a = Point(point.x, getY(point.x))
      val b = Point(getX(point.y), point.y)
      LineSegment(a,b).center.distance(point)
    }

    def isAbove(epsilon: Double)(point: Point): Boolean = {
      point.y > getY(point.x) + epsilon
    }
    def isBelow(epsilon: Double)(point: Point): Boolean = {
      point.y < getY(point.x) - epsilon
    }
  }
  case object Extremes {
    def fromPoints(points: List[Point]): Option[Extremes] = {
      if (points.isEmpty) None
      else {
        val x::xs = points
        Some(xs.foldLeft(Extremes(x, x, x, x)){ _ updateWith _ })
      }
    }
  }
  case class Extremes(minX: Point, maxX: Point, minY: Point, maxY: Point) {
    def updateWith(point: Point): Extremes = {
      copy(
        minX = if(point.x < minX.x) point else minX,
        maxX = if(point.x > maxX.x) point else maxX,
        minY = if(point.y < minY.y) point else minY,
        maxY = if(point.y > maxY.y) point else maxY
      )
    }

    lazy val distinctPoints = Set(minX,maxX,minY,maxY).toList
  }
  case object LinePartitioning {
    val Empty = LinePartitioning(Nil,Nil,Nil)
  }
  case class LinePartitioning(above: List[Point], on: List[Point], below: List[Point])

  def main(args: Array[String]): Unit = {
    val numberOfPoints = StdIn.readInt
    val points = (0 until numberOfPoints).map(_ => Point.readIntsFromLine(StdIn.readLine)).toList

    def expandIfPossible(from: Point, to: Point, points: List[Point], aboveLine: Boolean): List[LineSegment] = {
      if(from == to) Nil
      else expandConvexHull(LineSegment(from,to), points, aboveLine)
    }

    val convexHull = Extremes.fromPoints(points).map { extremes =>
      val Extremes(minX,maxX,minY,maxY) = extremes
      expandIfPossible(minX, maxY, points, aboveLine = true) ++
        expandIfPossible(maxY, maxX, points, aboveLine = true) ++
        expandIfPossible(maxX, minY, points, aboveLine = false) ++
        expandIfPossible(minY, minX, points, aboveLine = false)
    }.getOrElse(Nil)

    println(convexHull)
    println(convexHull.map(_.length).sum)
  }

  /**
    * Given an lineSegment that is part of the convex hull and points in the plane.
    * It will split the lineSegment in two lines, and tries to expand the convex hull of those lines recursively as well.
    * A line is split if there exists a point above/below the line, it will take the point with the maximum distance perpendicular to the line.
    */
  def expandConvexHull(lineSegment: LineSegment, points: Seq[Point], aboveLine: Boolean): List[LineSegment] = {
      val epsilon = 0.0002D
      val line = lineSegment.toLine
      val remainingPoints = if (aboveLine) points.filter(line.isAbove(epsilon)) else points.filter(line.isBelow(epsilon))

       println(s"points remaining $remainingPoints")

      if (remainingPoints.isEmpty) List(lineSegment)
      else {
        val furthestPoint = remainingPoints.maxBy(line.perpendicularDistance)
        val splitLineA = LineSegment(lineSegment.from, furthestPoint)
        val splitLineB = LineSegment(furthestPoint, lineSegment.to)

         println(s"Going to recursive call line: $splitLineA and $splitLineB with points $remainingPoints and aboveLine=$aboveLine")

        expandConvexHull(splitLineA, remainingPoints, aboveLine) ++ expandConvexHull(splitLineB, remainingPoints, aboveLine)
      }
  }
}
