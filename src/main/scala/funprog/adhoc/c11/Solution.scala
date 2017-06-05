package funprog.adhoc.c11

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import WatLogDomain._

object Solution {

  case object KnowledgeBase {
    val Empty = KnowledgeBase(List.empty)
  }
  case class KnowledgeBase(rules: List[Rule]) {

    def +(rule: Rule): KnowledgeBase = {
      this.copy(rule :: rules)
    }

//    def query(query: Query): Unit = {
//      query.terms.forall(satisfies)
//
////      query.terms.foldLeft(List.empty) { (agg, term) =>
////
////      }
//    }

//    def substituteVariable[A <: ComplexTerm](terms: List[A], variable: Variable, name: Name): List[A] = {
//      terms.map(term => term.substitute(variable, name))
//    }
//    def substituteVariable[A <: ComplexTerm](term: A, substitute: Variable, replacement: Name): ComplexTerm = {
//      term match {
//        case name: Name => name
//        case RelationalTerm(name, terms) => RelationalTerm(name, substituteVariable(terms, substitute, replacement))
//        case va: Variable if va == substitute => replacement
//        case va: Variable => va
//        case EqualityAssertion(first, second) => EqualityAssertion(substituteVariable(first, substitute, replacement), substituteVariable(second, substitute, replacement))
//        case NonEqualityAssertion(first, second) => NonEqualityAssertion(substituteVariable(first, substitute, replacement), substituteVariable(second, substitute, replacement))
//      }
//    }
//    def substituteVariable[A <: ComplexTerm](term: A, substitute: Variable, replacement: Name): ComplexTerm = {
//      term match {
//        case name: Name => name
//        case RelationalTerm(name, terms) => RelationalTerm(name, substituteVariable(terms, substitute, replacement))
//        case va: Variable if va == substitute => replacement
//        case va: Variable => va
//        case EqualityAssertion(first, second) => EqualityAssertion(substituteVariable(first, substitute, replacement), substituteVariable(second, substitute, replacement))
//        case NonEqualityAssertion(first, second) => NonEqualityAssertion(substituteVariable(first, substitute, replacement), substituteVariable(second, substitute, replacement))
//      }
//    }
//
//    def substituteVariable(terms: List[ComplexTerm], variable: Variable, term: RelationalTerm): List[ComplexTerm] = {
//      terms.map(term => term.substitute(variable, term))
//    }

//    def satisfies(rules: List[Rule])(query: Name): Boolean = {
//      rules match {
//        case Nil => false
//        case Rule(premises, con: Name) :: xs if con == query =>
//          premises.forall(satisfies(xs)) || satisfies(xs)(query)
//        case Rule(premises, con: Variable) :: xs =>
//          substituteVariable(premises, con, query).forall(satisfies(xs)) || satisfies(xs)(query)
//        case other :: xs => satisfies(xs)(query)
//      }
//    }
//    def satisfies(rules: List[Rule])(query: RelationalTerm): Boolean = {
//      rules match {
//        case Nil => false
//        case Rule(premises, con: RelationalTerm) :: xs if con == query =>
//          premises.forall(satisfies(xs)) || satisfies(xs)(query)
//        case Rule(premises, con: Variable) :: xs =>
//          substituteVariable(premises, con, query).forall(satisfies(xs)) || satisfies(xs)(query)
//        case other :: xs => satisfies(xs)(query)
//      }
//    }
//
//    //TODO variable assignment in term
//    //Rules are sorted in order you want to infer. (opposite insert order)
//    def satisfies(rules: List[Rule])(query: ComplexTerm): Boolean = {
//      def satisfies(rule: Rule): Boolean = {
//        rule -> query match {
//          case (Rule(premises, name: Name), Name(queryName)) => name == queryName && premises.forall(satisfies(rules.tail))
//          case (Rule(premises, Variable(name)), Name(queryName)) => premises
//        }
//      }
//
//      rules match {
//        case Nil => false
//        case x::xs =>
//      }
//
//
//      rules.foldLeft(List.empty[SimpleTerm]){ (conclusions,rule) =>
//
//
//        if(holds(rule.premises, conclusions)) {
//          rule.conclusion :: conclusions
//        } else {
//          conclusions
//        }
//      }
//    }

//    def holds(terms: List[ComplexTerm], facts: List[SimpleTerm]): Boolean = {
//      terms.forall {
//        case EqualityAssertion(first, second) => first == second
//        case NonEqualityAssertion(first, second) => first != second
//        case name: Name => facts.contains(name) //
//        case Variable(Name(name)) => true //ignored right now
//        case rt@RelationalTerm(name, terms) => facts.contains(rt)
//      }
//    }
//
//    def facts: List[SimpleTerm] = {
//      rules.foldLeft(List.empty[SimpleTerm]){ (conclusions,rule) =>
//        if(holds(rule.premises, conclusions)) {
//          rule.conclusion :: conclusions
//        } else {
//          conclusions
//        }
//      }
//  }


    /**
      * If a rule is a <simple-term> (a "fact"), it should be interpreted as stating that the term is true for all variable assignments.
      * Note that the fact syntax is merely syntax sugar for a rule with empty list of premises.
      * There are no semantic differences between FACT. and {() => FACT}.
      *
      * A <rule> should be interpreted as an inference rule.
      * The part in the parentheses lists the premises, and the part after the double arrow (=>) is the conclusion.
      * The premises may be <simple-term>s or <complex-term>s, while the conclusion is always a <simple-term>.
      * As with facts, all variables in a rule are considered to be universally quantified.
      * The rule states that if all premises can be proven true for some variable assignment, then the conclusion is also true for that variable assignment.
      */

//    def derivedRules(): List[Rule] {
//
//    }

  }

  def main(args: Array[String]): Unit = {
    handleInput(KnowledgeBase.Empty)
  }

  @tailrec
  def handleInput(knowledgeBase: KnowledgeBase): Unit = {
    val input: WatLogDomain.Op = WatLogParser.parseAllOrThrow(StdIn.readLine())
    input match {
      case rule:Rule =>
        println("Ok.")
        handleInput(knowledgeBase + rule)
      case query:Query =>
        println("Ready.")
        handleInput(knowledgeBase)
      case Command =>
        println("Bye.") //Terminate afterwards
      case NoOp(_) =>
        handleInput(knowledgeBase)
    }
  }

 // rule => substitute variables with terms =>

  // () => [r: a].
  // {([r: #z]) => [p: [r: #z], #z]}.


  // r -> a
  // if r -> #z then p -> (r -> #z) and #z  ///  if (r -> a)   p -> (r -> a) and a

  // ([p: [r: a], a])?     p -> (r -> a) and a ? true assign #z = a
  // ([p: #a, a])?      p -> #a and a ? true assign #a = (r -> a)
  // ([p: #a, b])?      p -> #a and b ? true assign #a = (r -> b)




  /**
    * A query is a conjunction of <complex-term>s, with all variables existentially quantified.
    * The goal of the WatLog system is to find all derivations and variable assignments such that the query holds true, using known facts and rules of inference.
    * To ensure well-specified evaluation order, query terms should be resolved left-to-right, and rules for resolution should be attempted in the same order they were entered into the system.
    */


}
