package funprog.adhoc.c1

import scala.annotation.tailrec
import scala.io.StdIn

/**
  * Jumping Bunnies
  * https://www.hackerrank.com/challenges/jumping-bunnies
  *
  * @author Frank van Meeuwen
  */
object Solution {

  def main(args: Array[String]): Unit = {
    val _ = StdIn.readInt()
    val jumpingDistances = StdIn.readLine().split(" ").map(_.toLong).toList
    println(leastCommonMultiple(jumpingDistances))
  }

  @tailrec
  def leastCommonMultiple(numbers: List[Long]): Long = {
    numbers match {
      case Nil => 0
      case x::Nil => x
      case x::y::ys => leastCommonMultiple(leastCommonMultiple(x,y)::ys)
    }
  }

  private def leastCommonMultiple(a: Long, b: Long): Long = (a / greatedCommonDivisor(a,b))*b

  @tailrec
  private def greatedCommonDivisor(a: Long, b: Long): Long = {
    b match {
      case 0 => a
      case _ => greatedCommonDivisor(b, a % b)
    }
  }
}
