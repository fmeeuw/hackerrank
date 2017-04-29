package misc

import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader

sealed trait Term
case class Variable(name: Char) extends Term
case class Abstraction(parameter: Variable, definition: Term) extends Term
case class Application(function: Term, argument: Term) extends Term

object LambdaParser extends RegexParsers with PackratParsers {

  def optionallyParenthized[A](parser: PackratParser[A]) = "(" ~> parser <~ ")" | parser

  lazy val variable: PackratParser[Variable] = """[a-z]{1}""".r ^^ { text => Variable(text.head) }
  lazy val abstraction: PackratParser[Abstraction] = optionallyParenthized { "λ" ~ variable ~ "." ~ term } ^^ { case _ ~ parameter ~ _ ~ definition => Abstraction(parameter, definition) }
  lazy val application: PackratParser[Application] =  optionallyParenthized { term ~ "-" ~ term } ^^ { case function ~ _ ~ argument => Application(function, argument)}
  lazy val term: PackratParser[Term] = application ||| abstraction ||| variable

  def parse(in: String): ParseResult[Term] = parse(term, new PackratReader(new CharSequenceReader(in.replace(' ', '-'))))
  def parseApplication(in: String): ParseResult[Application] = parse(application, new PackratReader(new CharSequenceReader(in.replace(' ', '-'))))

  override def skipWhitespace: Boolean = false
}


