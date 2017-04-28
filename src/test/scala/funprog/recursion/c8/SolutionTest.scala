package funprog.recursion.c8

import funprog.recursion.c8.Solution.{Extremes, LinePartitioning, LineSegment, Point}
import org.scalatest.{Matchers, WordSpec}

class SolutionTest extends WordSpec with Matchers {

  "The line's isAbove and isBelow function" should {

    "correctly work with integer points" in {
      val epsilon = 0.0D
      val points = for(x <- -3 to 3; y <- -1 to 1) yield Point(x,y)
      val line = LineSegment(Point(-3,-1), Point(3,1)).toLine
      val expectedAboveLine = points.filter(point => point.x < point.y*3).toList
      val expectedOnLine =  points.filter(point => point.x == point.y*3).toList
      val expectedBelowLine = points.filter(point => point.x > point.y*3).toList

      points.filter(line.isAbove(epsilon)) should contain theSameElementsAs expectedAboveLine
      points.filter(point => !line.isAbove(epsilon)(point) && !line.isBelow(epsilon)(point)) should contain theSameElementsAs expectedOnLine
      points.filter(line.isBelow(epsilon)) should contain theSameElementsAs expectedBelowLine
    }

    "partition double points correctly" in {
      val epsilon = 0.000002D
      val generatedPoints = for(x <- -3 to 3; y <- -1 to 1) yield Point(x+0.1*Math.random(),y+0.1*Math.random())
      val onLinePoints = for(i <- -3 to 3) yield Point(i*1.0D, i/3.0D)
      val line = LineSegment(Point(-3,-1), Point(3,1)).toLine
      val expectedAboveLine = generatedPoints.filter(point => point.y > (point.x/3 + epsilon)).toList
      val expectedOnLine =  onLinePoints ++ generatedPoints.filter(point => point.y >= (point.x/3 - epsilon) && point.y <= (point.x/3 + epsilon)).toList
      val expectedBelowLine = generatedPoints.filter(point => point.y < (point.x/3 - epsilon)).toList

      val points = generatedPoints ++ onLinePoints
      points.filter(line.isAbove(epsilon))should contain theSameElementsAs expectedAboveLine
      points.filter(point => !line.isAbove(epsilon)(point) && !line.isBelow(epsilon)(point)) should contain theSameElementsAs expectedOnLine
      points.filter(line.isBelow(epsilon)) should contain theSameElementsAs expectedBelowLine
    }

  }

  "The Extremes fromPoints function" should {

    "correctly calculate the extremes" in {
      val points = List(Point(1, 4), Point(2, 1), Point(2, 3), Point(2, 4),
        Point(2, 5), Point(3, 8), Point(3, 7), Point(3, 6), Point(3, 3), Point(4, 3),
        Point(4, 5), Point(6, 5))

      val expectedExtremes = Some(Extremes(Point(1,4), Point(6,5), Point(2,1), Point(3,8)))
      Extremes.fromPoints(points) shouldBe expectedExtremes
    }
  }

}
