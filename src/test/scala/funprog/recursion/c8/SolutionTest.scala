package funprog.recursion.c8

import funprog.recursion.c8.Solution.{LineSegment, Point}
import org.scalatest.{Matchers, WordSpec}

class SolutionTest extends WordSpec with Matchers {

  "The line and lineSegment theirs isAbove and isBelow function" should {

    "correctly work with integer points" in {
      val points = for(x <- -3 to 3; y <- -1 to 1) yield Point(x,y)
      val lineSegment = LineSegment(Point(-3,-1), Point(3,1))
      val expectedAboveLine = points.filter(point => point.x < point.y*3).toList
      val expectedOnLine =  points.filter(point => point.x == point.y*3).toList
      val expectedBelowLine = points.filter(point => point.x > point.y*3).toList

      points.filter(lineSegment.isAbove) should contain theSameElementsAs expectedAboveLine
      points.filter(point => !lineSegment.isAbove(point) && !lineSegment.isBelow(point)) should contain theSameElementsAs expectedOnLine
      points.filter(lineSegment.isBelow) should contain theSameElementsAs expectedBelowLine
    }

    "partition double points correctly" in {
      val generatedPoints = for(x <- -3 to 3; y <- -1 to 1) yield Point(x+0.1*Math.random(),y+0.1*Math.random())
      val onLinePoints = for(i <- -3 to 3) yield Point(i*1.0D, i/3.0D)
      val lineSegment = LineSegment(Point(-3,-1), Point(3,1))
      val expectedAboveLine = generatedPoints.filter(point => point.y > (point.x/3)).toList
      val expectedOnLine =  onLinePoints ++ generatedPoints.filter(point => point.y >= (point.x/3) && point.y <= (point.x/3)).toList
      val expectedBelowLine = generatedPoints.filter(point => point.y < (point.x/3)).toList

      val points = generatedPoints ++ onLinePoints

      points.filter(lineSegment.isAbove)should contain theSameElementsAs expectedAboveLine
      points.filter(point => !lineSegment.isAbove(point) && !lineSegment.isBelow(point)) should contain theSameElementsAs expectedOnLine
      points.filter(lineSegment.isBelow) should contain theSameElementsAs expectedBelowLine
    }

  }


  "The points distance method" should {

    "return the correct distance between two positive integer points" in {
      val from = Point(1,4)
      val to = Point(5,5)
      val expectedDistance = Math.sqrt(17)
      Point.distance(from,to) shouldEqual expectedDistance
      Point.distance(to,from) shouldEqual expectedDistance
    }

    "return the correct distance between two integer points" in {
      val from = Point(-3,-8)
      val to = Point(5,5)
      val expectedDistance = Math.sqrt(Math.pow(8,2) + Math.pow(13,2))
      Point.distance(from,to) shouldEqual expectedDistance
      Point.distance(to,from) shouldEqual expectedDistance
    }
  }

}
