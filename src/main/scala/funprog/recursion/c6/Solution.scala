package funprog.recursion.c6

import scala.io.StdIn

/**
  * String-o-Permute
  * https://www.hackerrank.com/challenges/string-o-permute
  */
object Solution {

  def main(args: Array[String]) {
    val nrOfCases = StdIn.readInt()
    (0 until nrOfCases).foreach { _ =>
      println(swap(StdIn.readLine().toList).mkString)
    }
  }

  def swap(text: List[Char]): List[Char] = text match {
    case x::y::ys => y::x::swap(ys)
    case x::Nil => List(x)
    case Nil => Nil
  }
}
