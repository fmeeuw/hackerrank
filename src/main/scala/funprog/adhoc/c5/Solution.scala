package funprog.adhoc.c5

import scala.io.StdIn

/**
  * Kundu And Bubble Wrap
  * https://www.hackerrank.com/challenges/kundu-and-bubble-wrap
  *
  * @author Frank van Meeuwen
  */
object Solution {

  def main(args: Array[String]): Unit = {
    val n::m::Nil = StdIn.readLine().split(" ").map(_.toInt).toList
    println(expectedPopAttempts(0, n*m))
  }

  def expectedPopAttempts(popped: Int, unpopped: Int): Double = {
    if(unpopped == 0) 0
    else {
      val total = popped + unpopped
      val successfullyPopChance = unpopped / total.toDouble
      val expectedNumberOfPops = 1 / successfullyPopChance
      expectedNumberOfPops + expectedPopAttempts(popped + 1, unpopped - 1)
    }
  }

}
