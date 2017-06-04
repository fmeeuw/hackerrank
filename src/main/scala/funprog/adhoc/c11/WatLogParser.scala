package funprog.adhoc.c11



import funprog.adhoc.c11.WatLogParser.simpleTerm

import scala.util.parsing.combinator.RegexParsers

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
  def noOp: Parser[NoOp] = "%" ~> comment ^^ { NoOp }
  def op: Parser[Op] = rule | query | command | noOp
  def inputLine: Parser[InputLine] = op <~ "\n" ^^ { InputLine }
}
