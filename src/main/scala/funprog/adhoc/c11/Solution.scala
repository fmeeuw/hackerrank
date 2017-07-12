package funprog.adhoc.c11

import scala.annotation.tailrec
import scala.io.StdIn
import WatLogDomain._

import scala.util.parsing.combinator.RegexParsers
import Solution.debug

object Solution {

  val Debug = false

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
                println("=====")
                assignments.foreach { case (vari,term) =>
                  println(s"$vari := $term")
                }
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

object WatLogDomain {

  type Assignments = Map[Variable, SimpleTerm]
  type Substitutions = Map[Variable, SimpleTerm]

  sealed trait MatchResult
  case object MatchResult {

    //TODO Test
    def sharedVariablesEqual(first: Map[Variable,SimpleTerm], second: Map[Variable,SimpleTerm]): Boolean = {
      second.forall {
        case (variable, term) => first.get(variable) match {
          case Some(firstTerm) if firstTerm != term => false
          case _ => true
        }
      }
    }

    case object NoMatch extends MatchResult
    case class Match(substitutions: Substitutions = Map.empty, assignments: Assignments = Map.empty) extends MatchResult {

      def +(other: MatchResult): MatchResult = {
        other match {
          case NoMatch => NoMatch
          case Match(otherSubstitutions, otherAssignments) =>
            if (sharedVariablesEqual(substitutions, otherSubstitutions) && sharedVariablesEqual(assignments, otherAssignments)) {
              Match(substitutions ++ otherSubstitutions, assignments ++ otherAssignments)
            } else {
              NoMatch
            }
        }
      }
    }
  }


  sealed trait QueryResult {
    def setAssignmentsSubstituteOld(assignments: Assignments, substitutions: Substitutions): QueryResult
    def ++(other: => QueryResult): QueryResult
  }
  case object QueryResult {

    case object NotSatisfied extends QueryResult {
      override def setAssignmentsSubstituteOld(assignments: Assignments, substitutions: Substitutions): QueryResult = this

      override def ++(other: => QueryResult): QueryResult = other
    }

    case object Satisfied {
      def apply(assignments: Assignments = Map.empty): Satisfied = Satisfied(List(assignments))
    }
    case class Satisfied(instances: List[Assignments]) extends QueryResult {
      override def setAssignmentsSubstituteOld(assignments: Map[Variable, SimpleTerm], substitutions: Substitutions): QueryResult = {
        this.copy(instances = instances.map(oldAssignments => assignments.mapValues(_.sub(substitutions ++ oldAssignments))))
      }

      override def ++(other: => QueryResult): QueryResult = other match {
        case NotSatisfied => this
        case Satisfied(otherInstances) => this.copy(instances = instances ++ otherInstances)
      }
    }

  }

  sealed trait ComplexTerm {

    /**
      * Substitutes the target variable of this complexTerm for the given complexTerm.
      */
    def sub(target: Variable, term: SimpleTerm): ComplexTerm

    /**
      * Applies all substitutions to this complexTerm.
      */
    def sub(substitutions: Map[Variable, SimpleTerm]): ComplexTerm = {
      substitutions.foldLeft(this){ case (agg,(target, term)) =>
        agg.sub(target, term)
      }
    }

    /**
      * Substitutes variables in the query which are present in the given assignments.
      */
    def substituteQueryAssignments(assignments: Assignments): ComplexTerm

    /**
      * Checks if this contains a variable.
      */
    def isFree: Boolean
  }

  sealed trait Assertion extends ComplexTerm {
    /**
      * Checks whether this term being used as query matches.
      * This term is expected to be free otherwise an assertion error is thrown.
      */
    def queryMatches(): Boolean

    /**
      * Collect all assignments that can be done to the variables in this term,
      * in order to possibly match.
      */
    def assignQueryVariables(): Assignments
  }
  //TODO have one case class for assertion and remove code duplication.
  case class EqualityAssertion(first: SimpleTerm, second: SimpleTerm) extends Assertion {
    override def toString: String = s"<$first = $second>"

    override def sub(target: Variable, term: SimpleTerm): EqualityAssertion = {
      EqualityAssertion(first.sub(target, term), second.sub(target, term))
    }

    override def queryMatches(): Boolean = {
      first == second
    }

    override def isFree: Boolean = first.isFree && second.isFree

    override def substituteQueryAssignments(assignments: Assignments): EqualityAssertion = {
      EqualityAssertion(first.substituteQueryAssignments(assignments), second.substituteQueryAssignments(assignments))
    }

    override def assignQueryVariables(): Assignments = {
      val firstAssignment = first.assignQueryVariables(second)
      if (firstAssignment.isEmpty) {
        second.assignQueryVariables(first)
      } else {
        firstAssignment
      }
    }
  }
  case class NonEqualityAssertion(first: SimpleTerm, second: SimpleTerm) extends Assertion {
    override def toString: String = s"<$first /= $second>"

    override def sub(target: Variable, term: SimpleTerm): NonEqualityAssertion = {
      NonEqualityAssertion(first.sub(target, term), second.sub(target, term))
    }

    override def queryMatches(): Boolean = {
      first != second
    }

    override def isFree: Boolean = first.isFree && second.isFree

    override def substituteQueryAssignments(assignments: Assignments): NonEqualityAssertion = {
      NonEqualityAssertion(first.substituteQueryAssignments(assignments), second.substituteQueryAssignments(assignments))
    }

    override def assignQueryVariables(): Assignments = {
      val firstAssignment = first.assignQueryVariables(second)
      if (firstAssignment.isEmpty) {
        second.assignQueryVariables(first)
      } else {
        firstAssignment
      }
    }
  }

  sealed trait SimpleTerm extends ComplexTerm {

    /**
      * Substitutes the target variable of this simpleTerm for the given simpleTerm.
      */
    def sub(target: Variable, term: SimpleTerm): SimpleTerm

    /**
      * Applies all substitutions to this complexTerm.
      */
    override def sub(substitutions: Map[Variable, SimpleTerm]): SimpleTerm = {
      substitutions.foldLeft(this){ case (agg,(target, term)) =>
        agg.sub(target, term)
      }
    }

    override def substituteQueryAssignments(assignments: Assignments): SimpleTerm

    /**
      * Collect all assignments that can be done to the variables in this term,
      * in order to possibly match the other simpleTerm.
      */
    def assignQueryVariables(other: SimpleTerm): Assignments

    /**
      * Checks whether this term being used as query, matches
      * the given simpleTerm. When it contains one or more variables,
      * we check that one variable cannot encode two different terms.
      *
      * For each variable occurring in the given other simpleTerm, a substitution
      * is returned.
      */
    def queryMatches(other: SimpleTerm): MatchResult

  }

  case class Name(name: String) extends SimpleTerm {
    override def toString: String = name

    override def sub(target: Variable, term: SimpleTerm): Name = this

    override def substituteQueryAssignments(assignments: Assignments): Name = this

    override def queryMatches(other: SimpleTerm): MatchResult = {
      other match {
        case otherName:Name if otherName == this => MatchResult.Match()
        case vari:Variable => MatchResult.Match(Map(vari -> this))
        case _ => MatchResult.NoMatch
      }
    }

    override val isFree: Boolean = false

    override def assignQueryVariables(other: SimpleTerm): Assignments = Map.empty
  }
  case class Variable(name: Name) extends SimpleTerm {
    override def toString: String = s"#$name"

    override def sub(target: Variable, term: SimpleTerm): SimpleTerm = {
      if(this == target) term else this
    }

    override def substituteQueryAssignments(assignments: Assignments): SimpleTerm = {
      assignments.get(this) match {
        case Some(substitution) => substitution
        case None => this
      }
    }

    override def queryMatches(other: SimpleTerm): MatchResult = {
      MatchResult.Match(assignments = Map(this -> other))
    }

    override val isFree: Boolean = true

    override def assignQueryVariables(other: SimpleTerm): Assignments = {
      Map(this -> other)
    }
  }
  case class RelationalTerm(name: Name, simpleTerms: List[SimpleTerm]) extends SimpleTerm {

    override def toString: String = s"[$name: ${simpleTerms.mkString(", ")}]"

    override def sub(target: Variable, term: SimpleTerm): RelationalTerm = {
      RelationalTerm(name, simpleTerms.map(_.sub(target, term)))
    }

    override def substituteQueryAssignments(assignments: Assignments): RelationalTerm = {
      copy(simpleTerms = simpleTerms.map(_.substituteQueryAssignments(assignments)))
    }

    override def queryMatches(other: SimpleTerm): MatchResult = {
      other match {
        case RelationalTerm(otherName, otherTerms) if this.name == otherName && this.simpleTerms.size == otherTerms.size =>
          this.simpleTerms
            .zip(otherTerms)
            .map { case (a: SimpleTerm, b: SimpleTerm) => a.queryMatches(b) }
            .foldLeft[MatchResult](MatchResult.Match()) { (agg, elem) =>
            agg -> elem match {
              case (m1: MatchResult.Match, m2:MatchResult) => m1+m2
              case _ => MatchResult.NoMatch
            }
          }
        case vari:Variable => MatchResult.Match(Map(vari -> this))
        case _ => MatchResult.NoMatch
      }
    }

    override def isFree: Boolean = simpleTerms.forall(elem => elem.isFree)

    override def assignQueryVariables(other: SimpleTerm): Assignments = {
      other match {
        case RelationalTerm(otherName, otherTerms) if this.name == otherName && this.simpleTerms.size == otherTerms.size =>
          this.simpleTerms
            .zip(otherTerms)
            .map { case (a: SimpleTerm, b: SimpleTerm) => a.assignQueryVariables(b)}
            .foldLeft(Map.empty[Variable, SimpleTerm]) { (agg, elem) =>
              //Note when multiple assignments are made to the same variable in the query
              //the first one is used.
              agg.++(elem)
            }
        case _ => Map.empty
      }
    }
  }

  sealed trait Op
  case class Rule(premises: List[ComplexTerm], conclusion: SimpleTerm) extends Op
  case class Query(terms: List[ComplexTerm]) extends Op
  case object Command extends Op
  case class NoOp(comment: Option[Comment]) extends Op
  case class Comment(comment: String)
}

object WatLogParser extends RegexParsers {
  import funprog.adhoc.c11.WatLogDomain._

  override def skipWhitespace: Boolean = false

  def name: Parser[Name] = """[a-zA-Z][a-zA-Z0-9\-]*""".r ^^ { Name }
  def variable: Parser[Variable] = "#" ~> name ^^ { Variable }
  def relationalTerm: Parser[RelationalTerm] = "[" ~> name ~ (": " ~> simpleTerms1 <~ "]") ^^ { case name ~ simpleTerms => RelationalTerm(name, simpleTerms)}
  def simpleTerm: Parser[SimpleTerm] = name | variable | relationalTerm
  def simpleTerms: Parser[List[SimpleTerm]] = repsep(simpleTerm, ", ")
  def simpleTerms1: Parser[List[SimpleTerm]] = rep1sep(simpleTerm, ", ")

  def equalityAssertion: Parser[EqualityAssertion] = "<" ~> simpleTerm ~ (" = " ~> simpleTerm <~ ">") ^^ { case term1 ~ term2 => EqualityAssertion(term1, term2)}
  def nonEqualityAssertion: Parser[NonEqualityAssertion] = "<" ~> simpleTerm ~ (" /= " ~> simpleTerm <~ ">") ^^ { case term1 ~ term2 => NonEqualityAssertion(term1, term2)}
  def complexTerm: Parser[ComplexTerm] = equalityAssertion | nonEqualityAssertion | simpleTerm
  def complexTerms: Parser[List[ComplexTerm]] = repsep(complexTerm, ", ")
  def complexTerms1: Parser[List[ComplexTerm]] = rep1sep(complexTerm, ", ")

  def fact: Parser[Rule] = simpleTerm <~ "." ^^ { Rule(List.empty, _) }
  def inferenceRule: Parser[Rule] = "{(" ~> complexTerms ~ (") => " ~> simpleTerm <~ "}.") ^^ { case precondition ~ conlcusion => Rule(precondition, conlcusion)}
  def rule: Parser[Rule] = fact | inferenceRule
  def query: Parser[Query] = "(" ~> complexTerms1 <~ ")?" ^^ { Query }

  def command: Parser[Command.type] = "quit!" ^^ { _ => Command }
  def comment: Parser[Comment] = "[^\n]*".r ^^ { Comment }
  def noOp: Parser[NoOp] = ("%" ~> comment).? ^^ { NoOp }
  def op: Parser[Op] = rule | query | command | noOp
  def inputLine: Parser[Op] = op <~ "\n"


  def parseAllOrThrow(line: String): Op = {
    val parseResult = parseAll(inputLine, line)
    parseResult match {
      case WatLogParser.Success(result, _) => result
      case WatLogParser.NoSuccess(msg, _) => throw new IllegalStateException(msg)
    }
  }
}

case object KnowledgeBase {
  val Empty = KnowledgeBase(List.empty)
}
case class KnowledgeBase(rules: List[Rule]) {

  def +(rule: Rule): KnowledgeBase = {
    this.copy(rules :+ rule)
  }

  def satisfies(query: Query): QueryResult = satisfiesAll(rules)(query.terms, Map.empty)

  private def satisfiesAll(rules: List[Rule])(queries: List[ComplexTerm], assignments: Assignments): QueryResult = {
    debug(s"satisfiesAll queries: $queries, $assignments")

    queries match {
      case Nil => QueryResult.Satisfied(assignments)
      case x::xs =>
        satisfies(rules)(x, assignments) match {
          case QueryResult.Satisfied(instances) =>
            debug(s"${instances.size} instances of assignments match query $x")
            instances.foldLeft[QueryResult](QueryResult.NotSatisfied) { (result, instanceAssignments) =>
              debug(s"Continuing with next query term, for an instance of assignments: $instanceAssignments")
              result ++ satisfiesAll(rules)(xs, instanceAssignments)
            }
          case QueryResult.NotSatisfied =>
            debug(s"Not satisfied for query $x")
            QueryResult.NotSatisfied
        }
    }
  }

  private def satisfies(rules: List[Rule])(query: ComplexTerm, assignments: Assignments): QueryResult = {

    //If the query has any variables that should be assigned, we substitute them first.
    query
      .substituteQueryAssignments(assignments) match {
      case simpleTerm: SimpleTerm => rules match {
        case Nil => QueryResult.NotSatisfied//(assignments)
        case rule :: xs => satisfiesRule(rule, xs)(simpleTerm, assignments) ++ satisfies(xs)(simpleTerm, assignments)

      }
      case assertion: Assertion =>
        val newAssignments = assertion.assignQueryVariables()
        assertion.queryMatches() match {
          case true => QueryResult.Satisfied(assignments ++ newAssignments)
          case false => QueryResult.NotSatisfied//(assignments)
        }
    }
  }

  /**
    * Checks if the current query term and assignments satisfy the given rule.
    * The query simpleTerm is expected to have all variables substituted that are present in the assignments.
    * When a rule has premises, it will check these premises against the remaining rules (tail).
    */
  private def satisfiesRule(rule: Rule, tail: List[Rule])(query: SimpleTerm, assignments: Assignments): QueryResult = {
    val Rule(premises, con) = rule
    debug(s"satisfiesRule: $rule, for query: $query and assignments: $assignments ")

    val newAssignments = query.assignQueryVariables(con) ++ assignments
    debug(s"newAssignments=$newAssignments")

    // The new assigments are also substituted in the query.
    // (The original assignments should allready have been substituted before calling this method.)
    val assignedQuery = query.substituteQueryAssignments(newAssignments)
    debug(s"assignedQuery=$assignedQuery")

    // Even after assigning, the query can contain variables.
    val matchResult = assignedQuery.queryMatches(con)
    debug(s"matchResult=$matchResult")

    matchResult match {
      case MatchResult.Match(substitutions, _) =>
        val substituedQuery = assignedQuery.sub(substitutions)
        substituedQuery match {
          case q: Name =>
            // When checking the premises, we don't want to mix the assignments to the original query with the new query.
            // So an empty map of assignments is passed and the result will have the original assignments and not the ones from checking the premises.
            val satisfiesResult = satisfiesAll(rules)(premises.map(_.sub(substitutions)), Map.empty)
            satisfiesResult.setAssignmentsSubstituteOld(newAssignments, substitutions)
          case q: RelationalTerm =>
            val satisfiesResult = satisfiesAll(rules)(premises.map(_.sub(substitutions)), Map.empty)
            satisfiesResult.setAssignmentsSubstituteOld(newAssignments, substitutions)
          case _ =>
            debug(s"Expected Name or RelationalTerm found $substituedQuery")
            QueryResult.NotSatisfied
        }

      case MatchResult.NoMatch => QueryResult.NotSatisfied
    }
  }
}