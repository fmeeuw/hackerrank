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
  private def handleInput(knowledgeBase: KnowledgeBase): Unit = {
    val parseResult = WatLogParser.parseAll(WatLogParser.inputLine, StdIn.readLine() + "\n")
    parseResult match {
      case WatLogParser.NoSuccess(msg, _) =>
        println(s"Unable to parse input!: $msg")
        handleInput(knowledgeBase)
      case WatLogParser.Success(result, _) => result match {
        case rule:Rule =>
          println("Ok.")
          handleInput(knowledgeBase + rule)
        case query:Query =>
          val queryResult = knowledgeBase.satisfies(query)
          queryResult match {
            case QueryResult.Satisfied(instances) =>
              instances.foreach { assignments =>
                println("SAT")
                println(assignments)
              }
            case QueryResult.NotSatisfied =>
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

}
