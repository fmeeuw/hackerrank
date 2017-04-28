package funprog.recursion.c3

import scala.io.StdIn

/**
  * Pascal's Triangle
  * https://www.hackerrank.com/challenges/pascals-triangle
  *
  * example:
  * 1
  * 1 1
  * 1 2 1
  * 1 3 3 1
  * 1 4 6 4 1
  *
  * @author Frank van Meeuwen
  */
object Solution {

  def main(args: Array[String]) {
    val nrOfRows = StdIn.readInt()
    val triangle = mkTriangle(nrOfRows)
    triangle.foreach(row => println(row.mkString(" ")))
  }

  def mkTriangle(nrOfRows: Int): List[List[Int]] = {
    def addAdjacents(numbers: List[Int]): List[Int] = {
      @scala.annotation.tailrec
      def addAdjacentsRec(todo: List[Int], acc: List[Int]): List[Int] = todo match {
        case x::y::xs => addAdjacentsRec(y::xs, acc:+x+y)
        case _ => acc
      }
      addAdjacentsRec(numbers, List.empty)
    }

    (1 until nrOfRows).foldLeft(List(List(1))) { (acc, _) =>
      acc:+addAdjacents(0+:acc.last:+0)
    }
  }

}