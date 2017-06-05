package funprog.adhoc.c11

import funprog.adhoc.c11.WatLogDomain.Variable

object WatLogDomain {

  sealed trait ComplexTerm {
    def sub(vari: Variable, term: SimpleTerm): ComplexTerm
  }
  case class EqualityAssertion(first: SimpleTerm, second: SimpleTerm) extends ComplexTerm {
    override def toString: String = s"<$first = $second>"

    override def sub(vari: Variable, term: SimpleTerm): EqualityAssertion = {
      EqualityAssertion(first.sub(vari, term), second.sub(vari, term))
    }
  }
  case class NonEqualityAssertion(first: SimpleTerm, second: SimpleTerm) extends ComplexTerm {
    override def toString: String = s"<$first /= $second>"

    override def sub(vari: Variable, term: SimpleTerm): NonEqualityAssertion = {
      NonEqualityAssertion(first.sub(vari, term), second.sub(vari, term))
    }
  }

  sealed trait SimpleTerm extends ComplexTerm {
    def sub(vari: Variable, term: SimpleTerm): SimpleTerm
  }

  case class Name(name: String) extends SimpleTerm {
    override def sub(vari: Variable, term: SimpleTerm): Name = this

    override def toString: String = name
  }
  case class Variable(name: Name) extends SimpleTerm {
    override def sub(vari: Variable, term: SimpleTerm): SimpleTerm = {
      if(this == vari) term else this
    }
    override def toString: String = s"#$name"
  }
  case class RelationalTerm(name: Name, simpleTerms: List[SimpleTerm]) extends SimpleTerm {
    override def sub(vari: Variable, term: SimpleTerm): RelationalTerm = {
      RelationalTerm(name, simpleTerms.map(_.sub(vari, term)))
    }
    override def toString: String = s"[$name: ${simpleTerms.mkString(", ")}]"
  }

  sealed trait Op
  case class Rule(premises: List[ComplexTerm], conclusion: SimpleTerm) extends Op
  case class Query(terms: List[ComplexTerm]) extends Op
  case object Command extends Op
  case class NoOp(comment: Comment) extends Op
  case class Comment(comment: String)
}
