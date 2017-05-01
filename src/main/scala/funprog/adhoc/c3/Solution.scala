package funprog.adhoc.c3

import scala.io.StdIn

/**
  * Remove Duplicates
  * https://www.hackerrank.com/challenges/remove-duplicates
  *
  * @author Frank van Meeuwen
  */
object Solution {

  def main(args: Array[String]) {
    val string = StdIn.readLine()
    println(removeDuplicates(string.toList).mkString)
  }

  def removeDuplicates(list: List[Char]): List[Char] = {
    list.foldLeft(List.empty[Char]){(seen,elem) =>
      if (!seen.contains(elem)) elem::seen
      else seen
    }.reverse
  }
}
