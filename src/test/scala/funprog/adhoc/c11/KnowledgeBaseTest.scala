package funprog.adhoc.c11

import funprog.adhoc.c11.WatLogDomain._
import org.scalatest.{Matchers, WordSpec}

class KnowledgeBaseTest extends WordSpec with Matchers {

  "The KnowledgeBase" should {

    "not satisfy a query when the knowledgeBase is empty" in {
      val kb = KnowledgeBase(List.empty)
      val query = Query(List(Name("a")))
      kb.satisfies(query) shouldBe QueryResult.NotSatisfied(Map.empty)
    }

    "always satisfies an empty query" in {
      val kb = KnowledgeBase(List.empty)
      val query = Query(List.empty)
      kb.satisfies(query) shouldBe QueryResult.Satisfied(Map.empty)
    }

    "satisfies a simple query for a name when it is available as fact." in {
      val name = Name("a")
      val kb = KnowledgeBase(List(Rule(List.empty, name)))
      val query = Query(List(name))
      kb.satisfies(query) shouldBe QueryResult.Satisfied(Map.empty)
    }

    "satisfies a simple query for a simple nested relational term when it is available as fact." in {
      val relationalTerm = RelationalTerm(Name("a"), List(Name("b"), RelationalTerm(Name("1"), List(Name("2"))), Name("d")))
      val kb = KnowledgeBase(List(Rule(List.empty, relationalTerm)))
      val query = Query(List(relationalTerm))
      kb.satisfies(query) shouldBe QueryResult.Satisfied(Map.empty)
    }

    "satisfies a simple query for a relational term, that is available as a fact, but requires nested variable substitution." in {
      val relationalTermFact = RelationalTerm(Name("a"), List(Name("b"), RelationalTerm(Name("1"), List(Variable(Name("2")))), Name("d")))
      val relationalTermQuery = RelationalTerm(Name("a"), List(Name("b"), RelationalTerm(Name("1"), List(Name("5"))), Name("d")))
      val kb = KnowledgeBase(List(Rule(List.empty, relationalTermFact)))
      val query = Query(List(relationalTermQuery))
      kb.satisfies(query) shouldBe QueryResult.Satisfied(Map.empty)
    }

    "satisfies a simple query for a name, when its can be substituted from a variable fact." in {
      val name = Name("a")
      val kb = KnowledgeBase(List(Rule(List.empty, Variable(Name("anything")))))
      val query = Query(List(name))
      kb.satisfies(query) shouldBe QueryResult.Satisfied(Map.empty)
    }

    "satisfies a simple query for a simple nested relational term, when it can be substituted from a variable fact." in {
      val relationalTerm = RelationalTerm(Name("a"), List(Name("b"), RelationalTerm(Name("1"), List(Name("2"))), Name("d")))
      val kb = KnowledgeBase(List(Rule(List.empty, Variable(Name("anything")))))
      val query = Query(List(relationalTerm))
      kb.satisfies(query) shouldBe QueryResult.Satisfied(Map.empty)
    }

    "assign variables from a query to the conclusion of a rule, even if those may be variables" in {
      val relationalTermQuery = RelationalTerm(Name("a"), List(Variable(Name("b")), RelationalTerm(Name("1"), List(Name("2"))), Name("d")))
      val relationalTermKb = RelationalTerm(Name("a"), List(Name("b"), RelationalTerm(Name("1"), List(Name("2"))), Name("d")))
      val kb = KnowledgeBase(List.empty) +
        Rule(List.empty, relationalTermKb) +
        Rule(List.empty, Variable(Name("c")))

      val query = Query(List(relationalTermQuery, Variable(Name("b")), Variable(Name("c"))))
      kb.satisfies(query) shouldBe QueryResult.Satisfied((Map(Variable(Name("c")) -> relationalTermKb, Variable(Name("b")) -> Name("b"))))
    }


    "satisfies a simple equality assertion of names in a query" in {
      val kb = KnowledgeBase.Empty
      val query = Query(List(EqualityAssertion(Name("a"), Name("a"))))
      kb.satisfies(query) shouldBe QueryResult.Satisfied(Map.empty)
    }



  }

  "The queryMatches function" should {

    "Not match a query to a term if the same variable would be assigned to different terms" in {
      val query = WatLogParser.parseAll(WatLogParser.relationalTerm, "[p: #c, #c]").get
      val term = WatLogParser.parseAll(WatLogParser.relationalTerm, "[p: [r: #z], #z]").get

      query.queryMatches(term) shouldBe MatchResult.NoMatch
    }

  }

}
