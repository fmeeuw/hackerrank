package funprog.adhoc.c7

import scala.io.StdIn

/**
  * Common Divisors
  * https://www.hackerrank.com/challenges/common-divisors
  *
  * @author Frank van Meeuwen
  */
object Solution {

  def main(args: Array[String]): Unit = {
    val testCases = StdIn.readInt()
    val numbers = (0 until testCases).map(_ => readIntTuple()).toList
    numbers.foreach{ case (a,b) => println(nrOfDivisorsOf(a,b)) }
  }

  private def readIntTuple(): (Int,Int) = {
    val a::b::Nil = StdIn.readLine().split(" ").map(_.toInt).toList
    a -> b
  }

  private def nrOfDivisorsOf(a: Int, b: Int): Int = {
    (1 to Math.min(a, b))
      .count(divisor => a % divisor == 0 && b % divisor == 0)
  }

}
