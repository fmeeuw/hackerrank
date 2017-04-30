package funprog.adhoc.c2

import scala.io.StdIn

/**
  * Rotate String
  * https://www.hackerrank.com/challenges/rotate-string?h_r=next-challenge&h_v=zen
  *
  * @author Frank van Meeuwen
  */
object Solution {

  def main(args: Array[String]): Unit = {
    val nrOfTestCases = StdIn.readInt()
    val testCases = (0 until nrOfTestCases).map(_ => StdIn.readLine())
    testCases.foreach { string =>
      println(allRotations(string.toList).map(_.mkString).mkString(" "))
    }
  }

  def allRotations(string: List[Char]): List[List[Char]] = {
    def generateRotations(string: List[Char], count: Int): List[List[Char]] = {
      if(count <= 0 ) Nil
      else {
        val result = string.tail :+ string.head
        result :: generateRotations(result, count - 1)
      }
    }
    generateRotations(string, string.length)
  }



}
