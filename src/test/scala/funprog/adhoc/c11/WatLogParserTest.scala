package funprog.adhoc.c11

import com.sun.org.apache.regexp.internal.RE
import funprog.adhoc.c11.WatLogDomain._
import org.scalatest.{Matchers, WordSpec}

class WatLogParserTest extends WordSpec with Matchers {

  def assertSuccessResult[A](parseResult: WatLogParser.ParseResult[A], expected: A): Unit = {
    parseResult match {
      case WatLogParser.Success(result, _) => result shouldBe expected
      case WatLogParser.NoSuccess(msg, _) => fail(s"parsing failed: $msg.")
    }
  }

  "The WatLog Parser" should {

    "be able to parse a name" in {
      val name = "hello-There-4123"
      val parseResult = WatLogParser.parseAll(WatLogParser.name, name)
      assertSuccessResult(parseResult, Name(name))
    }

    "not able to parse invalid names" in {
      WatLogParser.parseAll(WatLogParser.name, "4hello-there").successful shouldBe false
      WatLogParser.parseAll(WatLogParser.name, "-hello-there").successful shouldBe false
      WatLogParser.parseAll(WatLogParser.name, "*$hello-there").successful shouldBe false
    }

    "be able to parse a variable" in {
      val name = "hello-There-4123"
      val parseResult = WatLogParser.parseAll(WatLogParser.variable, "#" + name)
      assertSuccessResult(parseResult, Variable(Name(name)))
    }

    "be able to parse a relationalTerm" in {
      val name = "hello-there"
      val simpleTerms: List[SimpleTerm] = List(
        Name("hi"),
        Name("there"),
        Variable(Name("var1")),
        Variable(Name("var2")),
        Variable(Name("var3")),
        RelationalTerm(Name("rt1"), List(Name("whatever"))),
        RelationalTerm(Name("rt2"), List(Variable(Name("whatever")))),
        RelationalTerm(Name("rt3"), List(RelationalTerm(Name("rt4"), List(RelationalTerm(Name("rt5"), List(Name("booyaka")))))))
      )
      val parseResult = WatLogParser.parseAll(WatLogParser.relationalTerm, s"[$name: ${simpleTerms.map(_.toString).mkString(", ")}]")
      assertSuccessResult(parseResult, RelationalTerm(Name(name), simpleTerms))
    }

    "be able to parse an equalityAssertion" in {
      val term1 = Name("hello-there")
      val term2 = Variable(Name("kiekeboe"))
      val parseResult = WatLogParser.parseAll(WatLogParser.equalityAssertion, s"<$term1 = $term2>")
      assertSuccessResult(parseResult, EqualityAssertion(term1, term2))
    }

    "be able to parse an nonEqualityAssertion" in {
      val term1 = Name("hello-there")
      val term2 = Variable(Name("kiekeboe"))
      val parseResult = WatLogParser.parseAll(WatLogParser.nonEqualityAssertion, s"<$term1 /= $term2>")
      assertSuccessResult(parseResult, NonEqualityAssertion(term1, term2))
    }

    "be able to parse a complex term" in {
      val term1 = Name("hello-there")
      val term2 = Variable(Name("kiekeboe"))
      val parseResult = WatLogParser.parseAll(WatLogParser.complexTerm, s"<$term1 /= $term2>")
      assertSuccessResult(parseResult, NonEqualityAssertion(term1, term2))
    }

    "be able to parse a fact" in {
      val term1 = Name("fact")
      val parseResult = WatLogParser.parseAll(WatLogParser.rule, s"$term1.")
      assertSuccessResult(parseResult, Rule(List.empty, term1))
    }

    "be able to parse an inference rule." in {
      val term1 = Name("fact")
      val complexTerms = List(EqualityAssertion(Name("a"), Name("b")), Variable(Name("c")))
      val parseResult = WatLogParser.parseAll(WatLogParser.rule, s"{(${complexTerms.mkString(", ")}) => $term1}.")
      assertSuccessResult(parseResult, Rule(complexTerms, term1))
    }

    "be able to parse a query" in {
      val complexTerms =  List(EqualityAssertion(Name("a"), Name("b")), Variable(Name("c")))
      val parseResult = WatLogParser.parseAll(WatLogParser.query, s"(${complexTerms.mkString(", ")})?")
      assertSuccessResult(parseResult, Query(complexTerms))
    }

    "be able to parse a command" in {
      val parseResult = WatLogParser.parseAll(WatLogParser.command, "quit!")
      assertSuccessResult(parseResult, Command)
    }

    "be able to parse a comment" in {
      val comment = ";lkasd;lkas;d0r0-@%#%ASD;lm13r"
      val parseResult = WatLogParser.parseAll(WatLogParser.comment, comment)
      assertSuccessResult(parseResult, Comment(comment))
    }

    "not parse a newline as part of a comment" in {
      val comment = ";lkasd;lkas;d0r0-@%#%ASD;lm13r\n"
      val parseResult = WatLogParser.parseAll(WatLogParser.comment, comment)
      parseResult.successful shouldBe false
    }

    "be able to parse a no-op" in {
      val comment = "';lasd';lasd;l"
      val parseResult = WatLogParser.parseAll(WatLogParser.noOp, s"%$comment")
      assertSuccessResult(parseResult, NoOp(Comment(comment)))
    }

    "be able to parse a no-op as inputLine" in {
      val comment = "';lasd';lasd;l"
      val parseResult = WatLogParser.parseAll(WatLogParser.inputLine, s"%$comment\n")
      assertSuccessResult(parseResult, NoOp(Comment(comment)))
    }

    "be able to parse a command as inputLine" in {
      val parseResult = WatLogParser.parseAll(WatLogParser.inputLine, s"quit!\n")
      assertSuccessResult(parseResult, Command)
    }

  }

}
