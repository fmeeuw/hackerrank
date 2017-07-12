package funprog.adhoc.c11

import funprog.adhoc.c11.Solution.{debug}
import funprog.adhoc.c11.WatLogDomain._

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
          case QueryResult.Satisfied(updatedAssignments) =>
            debug(s"Combining assignments from multiple queries... ${updatedAssignments} with $assignments")
            satisfiesAll (rules) (xs, updatedAssignments ++ assignments)
          case QueryResult.NotSatisfied(_) =>
            debug(s"Not satisfied for query $x")
            QueryResult.NotSatisfied(assignments)
        }
    }
  }

  private def satisfies(rules: List[Rule])(query: ComplexTerm, assignments: Assignments): QueryResult = {

    //If the query has any variables that should be assigned, we substitute them first.
    query
      .substituteQueryAssignments(assignments) match {
      case simpleTerm: SimpleTerm => rules match {
        case Nil =>
          QueryResult.NotSatisfied(assignments)
        case rule :: xs =>
          satisfiesRule(rule, xs)(simpleTerm, assignments) || satisfies(xs)(simpleTerm, assignments)
      }
      case assertion: Assertion =>
        val newAssignments = assertion.assignQueryVariables()
        assertion.queryMatches() match {
          case true => QueryResult.Satisfied(assignments ++ newAssignments)
          case false => QueryResult.NotSatisfied(assignments)
        }
    }
  }

  // Note
  // When assigning variables in a query, they assigned term may contain variables.
  // They have to be substituted as well.

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

    //The new assigments are also substituted in the query, resuling in the query being free.
    // (The original assignments should allready have been substituted before calling this method.)
    // (Except when assigning non-free terms TODO)
    val assignedQuery = query.substituteQueryAssignments(newAssignments)
    debug(s"assignedQuery=$assignedQuery")

    val matchResult = assignedQuery.queryMatches(con)
    debug(s"matchResult=$matchResult")

    matchResult match {
      case MatchResult.Match(substitutions) =>
        val substituedQuery = assignedQuery.sub(substitutions)
        substituedQuery match {
          case q: Name =>
            // When checking the premises, we don't want to mix the assignments to the original query with the new query.
            // So an empty map of assignments is passed and the result will have the original assignments and not the ones from checking the premises.
            val satisfiesResult = satisfiesAll(rules)(premises.map(_.sub(substitutions)), Map.empty)
            satisfiesResult.withAssignments(assignments = newAssignments.mapValues(_.sub(substitutions ++ satisfiesResult.assignments)))
          case q: RelationalTerm =>
            val satisfiesResult = satisfiesAll(rules)(premises.map(_.sub(substitutions)), Map.empty)
            satisfiesResult.withAssignments(assignments = newAssignments.mapValues(_.sub(substitutions ++ satisfiesResult.assignments)))
          case _ =>
            debug(s"Expected Name or RelationalTerm found $substituedQuery")
            QueryResult.NotSatisfied(assignments)
        }

      case MatchResult.NoMatch => QueryResult.NotSatisfied(assignments)
    }
  }

//    debug(s"query with new assignments substituted= $query")
//    val result = (con, query, query.queryMatches(con)) match {
//      case (c: Name, q: Name, MatchResult.Match(substitutions)) =>
//        debug(s"query: $q satisfied conclusion $c => checking premises $premises")
//        // When checking the premises, we don't want to mix the assignments to the original query with the new query.
//        // So an empty map of assignments is passed and the result will have the original assignments and not the ones from checking the premises.
//        satisfiesAll(tail)(premises, Map.empty).copy(assignments = assignments)
//      case (c: Name, q: Variable, r:MatchResult) =>
//        debug(s"query: $q assigned to conclusion $c => checking premises $premises")
//        assignments.get(q) match {
//          // When checking the premises, we don't want to mix the assignments to the original query with the new query.
//          // So an empty map of assignments is passed and the result will have the original assignments and not the ones from checking the premises.
//          case None => satisfiesAll(tail)(premises, Map.empty).copy(assignments.updated(q, c))
//          case Some(assigned) => satisfies(rules)(assigned, assignments)    //This is a bit unfortunate because satisfies(xs, assignments)(query) is called as well, not sure when it occurs.
//        }
//      case (c: RelationalTerm, q: RelationalTerm, MatchResult.Match(substitutions)) =>
//        debug(s"query: $q matches signature of conclusion $c")
//
//        debug(s"substitutions=$substitutions")
//
//        if(substitutions.nonEmpty || newAssignments.nonEmpty) {
//          satisfiesAll(rule::tail)(premises.map(_.sub(substitutions)), Map.empty).copy(assignments = assignments ++ newAssignments.mapValues(_.sub(substitutions)))
//        } else {
//          if(c == q) satisfiesAll(tail)(premises, Map.empty).copy(assignments = assignments)
//          else QueryResult(assignments, false)
//        }
//      case (c: Variable, q: Name, MatchResult.Match(substitutions)) =>
//        debug(s"query: $q can substitute conclusion $c")
//        satisfiesAll(tail)(premises.map(_.sub(c, q)), Map.empty).copy(assignments = assignments)
//      case (c: Variable, q: RelationalTerm, r:MatchResult) =>
//        debug(s"query: $q can substitute conclusion $c")
//        satisfiesAll(tail)(premises.map(_.sub(c, q)), Map.empty).copy(assignments = assignments)
//      case (c: Variable, q: Variable, r:MatchResult) =>
//        debug(s"query: $q and conclusion $c are both variables, substituting $c for $q.")
//        satisfiesAll(tail)(premises.map(_.sub(c, q)), Map.empty).copy(assignments = assignments)
//      case (_, q: EqualityAssertion, MatchResult(matches, substitutions)) =>
//        debug(s"query: $q is EqualityAssertion!")
//        q match {
//          case EqualityAssertion(a: Name, b: Name) if a==b => QueryResult(assignments, true)
//          //Not sure what the specs are for variables in relational terms inside an equality assertion.
//          case EqualityAssertion(a: RelationalTerm, b: RelationalTerm) if a==b => QueryResult(assignments, true)
//          case EqualityAssertion(a: Variable, b: SimpleTerm) =>
//            assignments.get(a) match {
//              case None => satisfies(rules, assignments.updated(a, b))(query)
//              case Some(assigned) => satisfies(rules, assignments)(EqualityAssertion(assigned, b))
//            }
//          case EqualityAssertion(a: SimpleTerm, b: Variable) =>
//            assignments.get(b) match {
//              case None => satisfies(rules, assignments.updated(b, a))(query)
//              case Some(assigned) => satisfies(rules, assignments)(EqualityAssertion(a, assigned))
//            }
//          case other => QueryResult(assignments, false)
//        }
//      case (_, NonEqualityAssertion(a,b), MatchResult(matches, substitutions)) =>
//        debug(s"query: ${NonEqualityAssertion(a,b)} is NonEqualityAssertion!")
//        !satisfies(rules, assignments)(EqualityAssertion(a,b))
//      case _ => QueryResult(assignments, satisfies = false)
//    }
//
//    result
//  }

}

