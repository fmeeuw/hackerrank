package funprog.adhoc.c9

import scala.collection.mutable
import scala.io.StdIn

/**
  * Game of Kyles
  * https://www.hackerrank.com/challenges/game-of-kyles
  *
  * @author Frank van Meeuwen
  */
object Solution {

  type Configuration = List[Boolean]

  class CachedFunction[B,R](fun: CachedFunction[B,R] => B => R) extends Function[B,R] {
    var results = mutable.Map.empty[B,R]

    override def apply(v1: B): R = {
        results.get(v1) match {
          case None =>
            val result = fun(this)(v1)
            results += (v1 -> result)
            result
          case Some(result) => result
        }
    }
  }

  def main(args: Array[String]): Unit = {
    val grundyNumberCache = new CachedFunction[Int, Byte](grundyNumber)
    val nrOfTestCases = StdIn.readInt()
    val results: Seq[Boolean] = for (_ <- 0 until nrOfTestCases )
      yield {
        StdIn.readInt()
        val config = parseConfiguration(StdIn.readLine())
        val consecutivePins = toConsecutivePinsList(config)
        wins(grundyNumberCache)(consecutivePins)
      }
    results.foreach { win =>
      if(win) println("WIN") else println("LOSE")
    }
  }

  private def parseConfiguration(line: String): Configuration = {
    line.toCharArray.map {
      case 'X' => false
      case 'I' => true
      case other => throw new IllegalArgumentException(s"Unable to parse configuration from string '$line' it contains a char '$other' other than 'I' or 'X'.")
    }.toList
  }

  /**
    * Represents the config as a list of integers, where each integer represents
    * the number of consecutive standing pins.
    */
  private def toConsecutivePinsList(config: Configuration): List[Int] = {
    val consecutivePins = config.foldLeft(List(List.empty[Boolean]))((acc, standing) => {
      if(standing) (standing :: acc.head) :: acc.tail
      else List.empty[Boolean] :: acc
    })
    consecutivePins.filter(_.nonEmpty).map(_.size)
  }

  private def wins(grundyNumberFun: CachedFunction[Int, Byte])(pins: List[Int]): Boolean = {
    if(pins.isEmpty) false
    else pins.map(grundyNumber(grundyNumberFun)).reduce((a,b) => (a ^ b).toByte) != 0
  }

  /**
    * Calculates the grundy number for this number of consecutive pins.
    * The grundy number is calculated by taking all possible outcomes, by throwing either 1 or 2 pins.
    * For each outcome, that may consists of two rows of consecutive pins, the nim-sum of the grundy number of them is taken. (xor)
    * The grundy number for this number of pins is equal to the mex of all the nim-sums or zero for the end-condition.
    */
  private def grundyNumber(cachedGrundyNumber: CachedFunction[Int,Byte])(pins: Int): Byte = {
    if(pins == 0) 0
    else {
      val throwOnePin = (0 until pins).map(i => List(i, pins - i - 1))
      val throwTwoPins = (0 until pins - 1).map(i => List(i, pins - i - 2))
      val nimValues = (throwOnePin ++ throwTwoPins).map(_.map(cachedGrundyNumber))
      val nimSums = nimValues.map(_.reduce((a,b) => (a^b).toByte)).toSet
      mex(nimSums)
    }
  }

  /**
    * Calculates the minimum excludant, which is the smallest non-negative integer
    * not present in the given set.
    */
  private def mex(numbers: Set[Byte]): Byte = {
    def byteStream(from: Byte, step: Byte = 1): Stream[Byte] = {
      Stream.cons(from, byteStream((from+step).toByte, step))
    }

    byteStream(0)
      .map(byte => numbers.contains(byte) -> byte)
      .dropWhile { case (contains, _) => contains}
      .map { case (_, number) => number }
      .head
  }

}
