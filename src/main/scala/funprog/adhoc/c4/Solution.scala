package funprog.adhoc.c4

import scala.annotation.tailrec
import scala.io.StdIn

/**
  * Huge GCD
  * https://www.hackerrank.com/challenges/huge-gcd-fp
  *
  * @author Frank van Meeuwen
 */
object Solution {

  def main(args: Array[String]): Unit = {
    def readInts(): List[Int] = {
      StdIn.readLine.split(" ").map(_.toInt).toList
    }
    StdIn.readInt()
    val multiplesFormingA = readInts()
    StdIn.readInt()
    val multiplesFormingB = readInts()
    val a = multiplesFormingA.map(BigInt(_)).product
    val b = multiplesFormingB.map(BigInt(_)).product
    println(greatestCommonDivisor(a, b) % BigInt(1000000007))
  }

  @tailrec
  private def greatestCommonDivisor(a: BigInt, b: BigInt): BigInt = {
    if (b.equals(BigInt(0))) a
    else greatestCommonDivisor(b, a % b)
  }
}
