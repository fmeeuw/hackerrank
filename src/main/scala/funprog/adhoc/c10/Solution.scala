package funprog.adhoc.c10


import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.StdIn

/**
  * Subset Sum
  * https://www.hackerrank.com/challenges/subset-sum
  *
  * @author Frank van Meeuwen
  */
object Solution {

  trait CachedFunction[A,R] extends Function[A,R] {
    var results = mutable.Map.empty[(A),R]

    override def apply(v1: A): R = {
      results.get(v1) match {
        case None =>
          val result = fun(this)(v1)
          results += (v1 -> result)
          result
        case Some(result) => result
      }
    }

    def fun: CachedFunction[A,R] => A => R
  }
  class CachedSum(numbers: Vector[Long]) extends CachedFunction[Int, Long] {

    override def fun: (CachedFunction[Int, Long]) => Int => Long = sum

    /**
      * Calculates the sum of all numbers up until the given index.
      */
    private def sum(cachedSum: CachedFunction[Int, Long])(index: Int): Long = {
      if (index >= 0 && index < numbers.size) {
        cachedSum(index - 1) + numbers(index)
      } else {
        0
      }
    }

  }

  def main(args: Array[String]): Unit = {
    val n = StdIn.readLong()
    val numbers = readLongs().sortWith(_ > _).toVector
    val nrOfTestCases = StdIn.readInt()
    val sums = (0 until nrOfTestCases).map(_ => StdIn.readLong()).toList

    val cachedSum = new CachedSum(numbers)

    sums.foreach { sum =>
      minimalSubsetSum(numbers, sum, cachedSum) match {
        case None => println(-1)
        case Some(subset) => println(subset.size)
      }
    }
  }

  private def readLongs(): List[Long] = {
    StdIn.readLine().split(" ").map(_.toLong).toList
  }

  /**
    * Finds the minimal subset that is equal to or greater than the sum.
    * The numbers are expected to be sorted descending.
    *
    * Because we are interested in the *minimal* subset, whenever we have to choose a number to add to the subset,
    * we can just pick the highest number available.
    * When we sort the list descending, its a matter of picking an index in the list to split on.
    * This index we can find with binary search.
    */
  private def minimalSubsetSum(numbers: Vector[Long], sum: Long, cachedSum: CachedSum): Option[Vector[Long]] = {
    /**
      * Finds the minimal index i, for which the sum of elements numbers[0] + ... + numbers[i] >= sum.
      * @param index The index to start the search from
      * @param lowerBound The lower bound of the search range.
      * @param upperBound  The upper bound of the search range.
      */
    @tailrec
    def binarySearchRec(index: Int, lowerBound: Int, upperBound: Int): Option[Int] = {
      def step(start: Int, end: Int): Int = Math.max(1, (end-start)/2)

        if (cachedSum(index) >= sum) {
          val newIndex = index - step(lowerBound, index)
          if (newIndex >= lowerBound)
            binarySearchRec(newIndex, lowerBound, index)
          else
            Some(index)
        } else {
          val newIndex = index + step(index + 1, upperBound)
          if (newIndex <= upperBound)
            binarySearchRec(newIndex, index + 1, upperBound)
          else
            None
        }
      }

    binarySearchRec((numbers.size - 1) / 2, 0, numbers.size-1).map {
      index => numbers.take(index+1)
    }
  }
}
