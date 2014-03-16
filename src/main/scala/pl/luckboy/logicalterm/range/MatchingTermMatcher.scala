package pl.luckboy.logicalterm.range
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.logicalterm._

class MatchingTermMatcher extends Matcher[MatchingTerm]
{
  override def matchingTermFromTerm(term: Term): Option[MatchingTerm] =
    throw new UnsupportedOperationException
  
  private def checkSuperconjunctionNode(node: TermNode, rangeSets: Map[String, TermNodeRangeSet])(rangeSet: TermNodeRangeSet): Validation[FatalError, Option[(TermNodeRangeSet, Set[String])]] =
    node match {
      case TermBranch(childs) =>
        childs.foldLeft(some((rangeSet, Set[String]())).success[FatalError]) {
          case (Success(Some((newRangeSet, varNames))), child) =>
            checkSuperdisjunctionNode(child, rangeSets)(newRangeSet).map {
              _.flatMap {
                case (childRangeSet, childVarNames) => 
                  val newRangeSet2 = newRangeSet & childRangeSet
                  val varNames2 = varNames | childVarNames
                  if(!newRangeSet2.isEmpty) some((newRangeSet2, varNames2)) else none
              }
            }
          case (res, _)                                        =>
            res
        }
      case TermLeaf(varName) =>
        rangeSets.get(varName).map { rs => some((rs, Set(varName))).success }.getOrElse(FatalError("not found range set", NoPosition).failure)
    }
  
  private def checkSuperdisjunctionNode(node: TermNode, rangeSets: Map[String, TermNodeRangeSet])(rangeSet: TermNodeRangeSet): Validation[FatalError, Option[(TermNodeRangeSet, Set[String])]] =
    node match {
      case TermBranch(childs) =>
        childs.foldLeft(none[(TermNodeRangeSet, Set[String])].success[FatalError]) {
          case (Success(None), child) =>
            checkSuperdisjunctionNode(node, rangeSets)(rangeSet)
          case (res, _)               =>
            res
        }
      case TermLeaf(varName) =>
        checkSuperconjunctionNode(node, rangeSets)(rangeSet)
    }
  
  private def matchesTermsWithoutVarArgs(term1: MatchingTerm, term2: MatchingTerm, matching: Matching.Value) =
    matching match {
      case Matching.Terms             =>
        for {
          // superterm with term
          // superconjunctions with conjunctions
          optPair1 <- checkSuperconjunctionNode(term1.conjNode, term2.conjRangeSets)(TermNodeRangeSet.empty)
          // superdisjunctions with disjunctions
          optPair2 <- checkSuperdisjunctionNode(term2.conjNode, term1.disjRangeSets)(TermNodeRangeSet.empty)
          // term with superterm
          // conjunctions with superconjunctions
          optPair3 <- checkSuperconjunctionNode(term2.conjNode, term1.conjRangeSets)(TermNodeRangeSet.empty)
          // disjunctions with superdisjunctions
          optPair4 <- checkSuperdisjunctionNode(term1.conjNode, term2.disjRangeSets)(TermNodeRangeSet.empty)
        } yield  {
          for {
            (_, vns1)  <- optPair1; (_, vns2) <- optPair2
            (_, vns3)  <- optPair1; (_, vns4) <- optPair2
          } yield (vns1 | vns2 | vns3 | vns3)
        }
      case Matching.SupertermWithTerm =>
        for {
          // superconjunctions with conjunctions
          optPair1 <- checkSuperconjunctionNode(term1.conjNode, term2.conjRangeSets)(TermNodeRangeSet.empty)
          // superdisjunctions with disjunctions
          optPair2 <- checkSuperdisjunctionNode(term2.conjNode, term1.disjRangeSets)(TermNodeRangeSet.empty)
        } yield (for((_, vns1)  <- optPair1; (_, vns2) <- optPair2) yield (vns1 | vns2))
      case Matching.TermWithSuperterm =>
        for {
          // conjunctions with superconjunctions
          optPair1 <- checkSuperconjunctionNode(term2.conjNode, term1.conjRangeSets)(TermNodeRangeSet.empty)
          // disjunctions with superdisjunctions
          optPair2 <- checkSuperdisjunctionNode(term1.conjNode, term2.disjRangeSets)(TermNodeRangeSet.empty)
        } yield (for((_, vns1)  <- optPair1; (_, vns2) <- optPair2) yield (vns1 | vns2))
    }
    
  override def matches(term1: MatchingTerm, term2: MatchingTerm, matching: Matching.Value): Validation[FatalError, Boolean] =
    matchesTermsWithoutVarArgs(term1, term2, matching).flatMap {
      _.map {
        varNames =>
          varNames.foldLeft(true.success[FatalError]) {
            case (Success(true), varName) =>
              (term1.varArgs.get(varName) |@| term2.varArgs.get(varName)) {
                (args1, args2) =>
                  if(args1.size === args2.size)
                    args1.zip(args2).foldLeft(true.success[FatalError]) {
                      case (Success(true), (arg1, arg2)) => matches(arg1, arg2, Matching.Terms)
                      case (res, _)                      => res
                    }
                  else
                    false.success
              }.getOrElse(FatalError("not found variable arguments", NoPosition).failure)
            case (res, _)                 =>
              res
          }
      }.getOrElse(false.success)
    }
}