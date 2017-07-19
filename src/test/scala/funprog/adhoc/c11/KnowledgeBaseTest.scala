package funprog.adhoc.c11

import funprog.adhoc.c11.WatLogDomain._
import org.scalatest.{Matchers, WordSpec}

class KnowledgeBaseTest extends WordSpec with Matchers {

  "The KnowledgeBase" should {

    "not satisfy a query when the knowledgeBase is empty" in {
      val kb = KnowledgeBase(List.empty)
      val query = Query(List(Name("a")))
      kb.satisfies(query) shouldBe QueryResult.NotSatisfied
    }

    "always satisfies an empty query" in {
      val kb = KnowledgeBase(List.empty)
      val query = Query(List.empty)
      kb.satisfies(query) shouldBe QueryResult.Satisfied()
    }

    "satisfies a simple query for a name when it is available as fact." in {
      val name = Name("a")
      val kb = KnowledgeBase(List(Rule(List.empty, name)))
      val query = Query(List(name))
      kb.satisfies(query) shouldBe QueryResult.Satisfied()
    }

    "satisfies a simple query for a simple nested relational term when it is available as fact." in {
      val relationalTerm = RelationalTerm(Name("a"), List(Name("b"), RelationalTerm(Name("1"), List(Name("2"))), Name("d")))
      val kb = KnowledgeBase(List(Rule(List.empty, relationalTerm)))
      val query = Query(List(relationalTerm))
      kb.satisfies(query) shouldBe QueryResult.Satisfied()
    }

    "satisfies a simple query for a relational term, that is available as a fact, but requires nested variable substitution." in {
      val relationalTermFact = RelationalTerm(Name("a"), List(Name("b"), RelationalTerm(Name("1"), List(Variable(Name("2")))), Name("d")))
      val relationalTermQuery = RelationalTerm(Name("a"), List(Name("b"), RelationalTerm(Name("1"), List(Name("5"))), Name("d")))
      val kb = KnowledgeBase(List(Rule(List.empty, relationalTermFact)))
      val query = Query(List(relationalTermQuery))
      kb.satisfies(query) shouldBe QueryResult.Satisfied()
    }

    "satisfies a simple query for a name, when its can be substituted from a variable fact." in {
      val name = Name("a")
      val kb = KnowledgeBase(List(Rule(List.empty, Variable(Name("anything")))))
      val query = Query(List(name))
      kb.satisfies(query) shouldBe QueryResult.Satisfied()
    }

    "satisfies a simple query for a simple nested relational term, when it can be substituted from a variable fact." in {
      val relationalTerm = RelationalTerm(Name("a"), List(Name("b"), RelationalTerm(Name("1"), List(Name("2"))), Name("d")))
      val kb = KnowledgeBase(List(Rule(List.empty, Variable(Name("anything")))))
      val query = Query(List(relationalTerm))
      kb.satisfies(query) shouldBe QueryResult.Satisfied()
    }

    "assign variables from a query to the conclusion of a rule, even if those may be variables" in {
      val relationalTermQuery = RelationalTerm(Name("a"), List(Variable(Name("b")), RelationalTerm(Name("1"), List(Name("2"))), Name("d")))
      val relationalTermKb = RelationalTerm(Name("a"), List(Name("b"), RelationalTerm(Name("1"), List(Name("2"))), Name("d")))
      val kb = KnowledgeBase(List.empty) +
        Rule(List.empty, relationalTermKb) +
        Rule(List.empty, Variable(Name("c")))

      val query = Query(List(relationalTermQuery, Variable(Name("b")), Variable(Name("c"))))
      kb.satisfies(query) shouldBe QueryResult.Satisfied(List(
        Map(Variable(Name("c")) -> relationalTermKb, Variable(Name("b")) -> Name("b")),
        Map(Variable(Name("c")) -> relationalTermKb, Variable(Name("b")) -> relationalTermKb)
      ))
    }


    "satisfies a simple equality assertion of names in a query" in {
      val kb = KnowledgeBase.Empty
      val query = Query(List(Assertion(Name("a"), Name("a"), true)))
      kb.satisfies(query) shouldBe QueryResult.Satisfied()
    }

    "satisfies a equality assertion that need a variable to be assigned." in {
      val kb = KnowledgeBase(List(
      WatLogParser.parseAll(WatLogParser.rule, "[p: a].").get,
      WatLogParser.parseAll(WatLogParser.rule, "{([p: #x], <[r: #x] = #y>) => [q: #x, #y]}.").get
      ))

      val query = WatLogParser.parseAll(WatLogParser.query, "([q: #x, #y])?").get

      val expectedAssignments = Map(
        Variable(Name("x")) -> Name("a"),
        Variable(Name("y")) -> RelationalTerm(Name("r"), List(Name("a")))
      )

      kb.satisfies(query) shouldBe QueryResult.Satisfied(assignments = expectedAssignments)
    }

    "return multiple assignments that match, when multiple matches are possible." in {
      val kb = KnowledgeBase(List(
        WatLogParser.parseAll(WatLogParser.rule, "[witnessed-by-at-on: ColTravis, Martha, CountryHouse, Tuesday].").get,
        WatLogParser.parseAll(WatLogParser.rule, "[witnessed-by-at-on: ColTravis, Martha, CountryHouse, Wednesday].").get
      ))
      val query = WatLogParser.parseAll(WatLogParser.query, "([witnessed-by-at-on: ColTravis, Martha, CountryHouse, #x])?").get
      kb.satisfies(query) shouldBe QueryResult.Satisfied(List(
        Map(Variable(Name("x")) -> Name("Tuesday")),
        Map(Variable(Name("x")) -> Name("Wednesday"))
      ))
    }




  }

  "The queryMatches function" should {

    "Not match a query to a term if the same variable would be assigned to different terms" in {
      val query = WatLogParser.parseAll(WatLogParser.relationalTerm, "[p: #c, #c]").get
      val term = WatLogParser.parseAll(WatLogParser.relationalTerm, "[p: [r: #z], #z]").get

      query.queryMatches(term) shouldBe MatchResult.NoMatch
    }

    "Not match a query to a term if the same variable occurs in a relation term." in {
      val query = WatLogParser.parseAll(WatLogParser.variable, "#c").get
      val term = WatLogParser.parseAll(WatLogParser.relationalTerm, "[p: #c]").get

      query.queryMatches(term) shouldBe MatchResult.NoMatch
      term.queryMatches(query) shouldBe MatchResult.NoMatch
    }

    "Match a relational term to another relational term with variables in it." in {
      val query = WatLogParser.parseAll(WatLogParser.relationalTerm, "[p: #x, #y]").get
      val term = WatLogParser.parseAll(WatLogParser.relationalTerm, "[p: #x, #y]").get

      query.queryMatches(term) shouldBe MatchResult.Match(assignments = Map(Variable(Name("x"))->Variable(Name("x")), Variable(Name("y"))->Variable(Name("y"))))
    }

  }

}
