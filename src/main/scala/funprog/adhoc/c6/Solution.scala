package funprog.adhoc.c6

import scala.io.StdIn

/**
  * Missing Numbers (FP)
  * https://www.hackerrank.com/challenges/missing-numbers-fp
  *
  * @author Frank van Meeuwen
  */
object Solution {

  def main(args: Array[String]): Unit = {
    StdIn.readInt()
    val firstList = readInts
    StdIn.readLine()
    val secondList = readInts
    println(findMissingNumbers(firstList,secondList).toList.sorted.mkString(" "))
  }

  def findMissingNumbers(firstList: List[Int], secondList: List[Int]): Set[Int] = {
    diff(occurencesMap(firstList), occurencesMap(secondList))
  }

  private def occurencesMap(list: List[Int]): Map[Int,Int] = {
    list.foldLeft(Map.empty[Int,Int]){(acc,elem) =>
      acc.get(elem) match {
        case Some(occurrences) => acc.updated(elem, occurrences+1)
        case None => acc + (elem -> 1)
      }
    }
  }

  private def diff(first: Map[Int,Int], second: Map[Int,Int]): Set[Int] = {
    second.flatMap { case (elem,secondOccurences) =>
      val firstOccurences = first.getOrElse(elem, 0)
      if (firstOccurences != secondOccurences) Some(elem)
      else None
    }.toSet
  }

  private def readInts = {
    StdIn.readLine().split(" ").map(_.toInt).toList
  }
}
