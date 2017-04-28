package funprog.recursion.c5

import scala.annotation.tailrec
import scala.io.StdIn

/**
  * String Mingling
  * https://www.hackerrank.com/challenges/string-mingling
  *
  * @author Frank van Meeuwen
  */
object Solution {

  def main(args: Array[String]) {
    val first = StdIn.readLine()
    val second = StdIn.readLine()
    println(mingle(first.toList, second.toList).reverse.mkString)
  }

  @tailrec
  def mingle(restA: List[Char], restB: List[Char], acc: List[Char] = List.empty): List[Char] = {
    if(restA.isEmpty || restB.isEmpty) acc
    else { mingle(restA.tail, restB.tail, restB.head :: restA.head :: acc) }
  }
}

