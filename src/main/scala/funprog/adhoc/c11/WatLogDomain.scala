package funprog.adhoc.c11

object WatLogDomain {

  sealed trait ComplexTerm
  case class EqualityAssertion(first: SimpleTerm, second: SimpleTerm) extends ComplexTerm {
    override def toString: String = s"<$first = $second>"
  }
  case class NonEqualityAssertion(first: SimpleTerm, second: SimpleTerm) extends ComplexTerm {
    override def toString: String = s"<$first /= $second>"
  }

  sealed trait SimpleTerm extends ComplexTerm
  case class Name(name: String) extends SimpleTerm {
    override def toString: String = name
  }
  case class Variable(name: Name) extends SimpleTerm {
    override def toString: String = s"#$name"
  }
  case class RelationalTerm(name: Name, simpleTerms: List[SimpleTerm]) extends SimpleTerm {
    override def toString: String = s"[$name: ${simpleTerms.mkString(", ")}]"
  }

  sealed trait Op
  case class Rule(premises: List[ComplexTerm], conclusion: SimpleTerm) extends Op
  case class Query(terms: List[ComplexTerm]) extends Op
  case object Command extends Op
  case class NoOp(comment: Comment) extends Op
  case class Comment(comment: String)
  case class InputLine(op: Op)
}
