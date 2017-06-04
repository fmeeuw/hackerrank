package funprog.adhoc.c11

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object Solution {

  def main(args: Array[String]): Unit = {
    val x:RegexParsers = new RegexParsers {

    }
    x.regex(new Regex("(S|s)cala"))
    println("kiekeboe")
  }

}
