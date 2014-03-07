package pl.luckboy.logicalterm.undistributed
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.logicalterm._

class MatchingTermMatcher extends Matcher[MatchingTerm]
{
  override def matchingTermFromTerm(term: Term): Option[MatchingTerm] =
    throw new UnsupportedOperationException

  private def matchesSupertermNodeWithTermNode(node1: TermNode, term1: MatchingTerm, node2: TermNode, term2: MatchingTerm): Validation[FatalError, Option[Set[String]]] = {
    // node1 is superconjunction node or disjunction node
    // node2 is superdisjunction node or conjunction node
    (node1, node2) match {
      case (TermNode(childs1, firstVarName1), TermNode(childs2, firstVarName2)) if childs1.isEmpty =>
        val res = if(firstVarName1 =/= firstVarName2)
          term2.varIdxs.get(firstVarName1) match {
            case Some(varIdxs2) => 
              varIdxs2.foldLeft(false) {
                (b2, varIdx2) => b2 | childs2.contains(TermNodeRange(varIdx2, varIdx2))
              }.success
            case None           =>
              FatalError("not found variable", NoPosition).failure
          }
        else
          true.success
        res.map { if(_) some(Set(firstVarName1)) else none }
      case (TermNode(childs1, _), TermNode(childs2, _)) if (!childs1.isEmpty) && (!childs2.isEmpty) =>
        childs1.values.foldLeft(some(Set[String]()).success[FatalError]) {
          case (Success(Some(varNames)), child1) =>
            term2.varIdxs.get(child1.firstVarName) match {
              case Some(varIdxs2) =>
                varIdxs2.foldLeft(some(varNames).success[FatalError]) {
                  case (Success(Some(varNames2)), varIdx2) =>
                    childs2.get(TermNodeRange(varIdx2, varIdx2)) match {
                      case Some(child2) => matchesSupertermNodeWithTermNode(child2, term2, child1, term1).map { _.map { varNames2 | _ } }
                      case None         => none.success
                    }
                  case (res, _)                            =>
                    res
                }
              case None           =>
                FatalError("not found variable", NoPosition).failure
            }
          case (res, _)                          =>
            res
        }
      case _ =>
        none.success
    }
  }
  
  private def matchesConjuctionNodes(node1: TermNode, term1: MatchingTerm, node2: TermNode, term2: MatchingTerm, matching: Matching.Value) =
    matching match {
      case Matching.Terms =>
        for {
          varNames1 <- matchesSupertermNodeWithTermNode(node1, term1, node2, term2)
          varNames2 <- matchesSupertermNodeWithTermNode(node2, term2, node1, term1)
        } yield {
          (varNames1 |@| varNames2) { _ | _ }
        }
      case Matching.SupertermWithTerm =>
        matchesSupertermNodeWithTermNode(node1, term1, node2, term2)
      case Matching.TermWithSuperterm =>
        matchesSupertermNodeWithTermNode(node2, term2, node1, term1)
    }
  
  override def matches(term1: MatchingTerm, term2: MatchingTerm, matching: Matching.Value) = {
    // term1.root is superconjunction node
    // term2.root is conjunction node
    matchesConjuctionNodes(term1.root, term1, term2.root, term2, matching).flatMap {
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
}