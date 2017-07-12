package funprog.adhoc.c11

import scala.annotation.tailrec
import scala.io.StdIn
import WatLogDomain._

object Solution {

  val Debug = true

  def debug(line: String): Unit = {
    if(Debug) println(line)
  }

  def main(args: Array[String]): Unit = {
    handleInput(KnowledgeBase.Empty)
  }

  @tailrec
  def handleInput(knowledgeBase: KnowledgeBase): Unit = {
    val input: WatLogDomain.Op = WatLogParser.parseAllOrThrow(StdIn.readLine() + "\n")
    input match {
      case rule:Rule =>
        println("Ok.")
        handleInput(knowledgeBase + rule)
      case query:Query =>
        val queryResult = knowledgeBase.satisfies(query)
        queryResult match {
          case QueryResult.Satisfied(assignments) =>
            println("SAT")
            println(assignments)
          case QueryResult.NotSatisfied(_) =>
            println("UNSAT")
        }
        println("Ready.")
        handleInput(knowledgeBase)
      case Command =>
        println("Bye.") //Terminate afterwards
      case NoOp(_) =>
        handleInput(knowledgeBase)
    }
  }

}
