package funprog.adhoc.c11

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
//  sealed trait SingleQueryResult {
//    def assignments: Map[Variable, SimpleTerm]
//    def withAssignments(assignments: Map[Variable, SimpleTerm]): QueryResult
//  }
//  case object SingleQueryResult {
//    case class NotSatisfied(assignments: Map[Variable, SimpleTerm]) extends QueryResult {
//      override def ||(other: => QueryResult): QueryResult = other
//
//      override def withAssignments(newAssignments: Map[Variable, SimpleTerm]): QueryResult = {
//        this.copy(newAssignments)
//      }
//    }
//    case class Satisfied(assignments: List[Map[Variable, SimpleTerm]]) extends QueryResult {
//      override def ||(other: => QueryResult): QueryResult = this
//
//      override def withAssignments(newAssignments: Map[Variable, SimpleTerm]): QueryResult = {
//        this.copy(newAssignments)
//      }
//    }
//  }
//
//  sealed trait QueryResult {
//    def ||(other: => QueryResult): QueryResult
//    def assignments: Map[Variable, SimpleTerm]
//    def withAssignments(assignments: Map[Variable, SimpleTerm]): QueryResult
//  }
//  case object QueryResult {
//
//    case class NotSatisfied(assignments: Map[Variable, SimpleTerm]) extends QueryResult with SingleQueryResult {
//      override def ||(other: => QueryResult): QueryResult = other
//
//      override def withAssignments(newAssignments: Map[Variable, SimpleTerm]): QueryResult = {
//        this.copy(newAssignments)
//      }
//    }
//
//
//  }

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
