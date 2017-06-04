package funprog.recursion.c9

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.StdIn

/**
  * String Compression
  * https://www.hackerrank.com/challenges/string-compression
  *
  * @author Frank van Meeuwen
  */
object Solution {

  def main(args: Array[String]): Unit = {
    val message = StdIn.readLine()
    println(encode(message.toList))
  }

  case class CharFrequency(char: Char, frequency: Int) {
    def plusOne = copy(frequency = frequency + 1)

    override lazy val toString: String = if (frequency <= 1) s"$char" else s"$char$frequency"
  }

  def encode(message: List[Char]): String = {
    @tailrec
    def encodeRecursively(message: List[Char], previous: CharFrequency, encoded: List[CharFrequency]): List[CharFrequency] = {
      message match {
        case Nil => previous :: encoded
        case x :: xs if (x == previous.char) => encodeRecursively(xs, previous.plusOne, encoded)
        case x :: xs if (x != previous.char) => encodeRecursively(xs, CharFrequency(x, 1), previous :: encoded)
      }
    }

    val charFrequencies: Seq[Solution.CharFrequency] = message match {
      case Nil => Nil
      case x :: xs => encodeRecursively(xs, CharFrequency(x, 1), Nil)
    }

    charFrequencies.reverseIterator.map(_.toString).mkString
  }


}
