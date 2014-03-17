package pl.luckboy.logicalterm.range
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.logicalterm._

class MatchingTermMatcher extends Matcher[MatchingTerm]
{
  private def matchingTermNodeFromTermForDepth(term: Term, depth: Int): Option[MatchingTerm] = {
    throw new UnsupportedOperationException
  }
  
  override def matchingTermFromTerm(term: Term): Option[MatchingTerm] =
    throw new UnsupportedOperationException
  
  private def fullRangeSetPair = (TermNodeRangeSet.full, TermNodeRangeSet.full)
    
  private def checkSuperconjunctionNode(node: TermNode, rangeSets: Map[String, TermNodeRangeSet], depthRangeSets: List[TermNodeRangeSet])(rangeSetPair: (TermNodeRangeSet, TermNodeRangeSet)): Validation[FatalError, Option[((TermNodeRangeSet, TermNodeRangeSet), Set[String])]] = {
    depthRangeSets match {
      case depthRangeSet :: nextDepthRangeSet :: _ =>
        val (rangeSet, rangeSubset) = rangeSetPair
        val rangeSet2 = rangeSet & rangeSubset.superset(nextDepthRangeSet)
        val rangeSubset2 = rangeSubset & nextDepthRangeSet.subset(nextDepthRangeSet)
        (node match {
          case TermBranch(childs) =>
            childs.foldLeft(some(((rangeSet2, rangeSubset2), Set[String]())).success[FatalError]) {
              case (Success(Some((newRangeSetPair, varNames))), child) =>
                val (newRangeSet, newRangeSubset) = newRangeSetPair
                checkSuperdisjunctionNode(child, rangeSets, depthRangeSets)(newRangeSetPair).map {
                  _.flatMap {
                    case (childRangeSetPair, childVarNames) =>
                      val (childRangeSet, childRangeSubset) = childRangeSetPair
                      val newRangeSet2 = newRangeSet & childRangeSet
                      val newRangeSubset2 = newRangeSubset & childRangeSubset
                      val varNames2 = varNames | childVarNames
                      if(!newRangeSet2.isEmpty && !newRangeSubset2.isEmpty)
                        some(((newRangeSet2, newRangeSubset2), varNames2))
                      else
                        none
                  }
                }
              case (res, _)                                            =>
                res
            }
          case TermLeaf(_) =>
            checkSuperdisjunctionNode(node, rangeSets, depthRangeSets)(rangeSetPair)
        }).map {
          _.map {
            case ((rangeSet3, rangeSubset3), varNames3) =>
              val rangeSet4 = rangeSet3 | rangeSubset.superset(depthRangeSet)
              val rangeSubset4 = rangeSubset3 | rangeSet.subset(depthRangeSet)
              ((rangeSet4, rangeSubset4), varNames3)
          }
        }
      case _ :: Nil =>
        FatalError("too few list of depth range sets", NoPosition).failure
      case Nil =>
        FatalError("empty list of depth range sets", NoPosition).failure
    }
  }
  
  private def checkSuperdisjunctionNode(node: TermNode, rangeSets: Map[String, TermNodeRangeSet], depthRangeSets: List[TermNodeRangeSet])(rangeSetPair: (TermNodeRangeSet, TermNodeRangeSet)): Validation[FatalError, Option[((TermNodeRangeSet, TermNodeRangeSet), Set[String])]] =
    depthRangeSets match {
      case _ :: (depthRangeSets2 @ (nextDepthRangeSet :: _)) =>
        node match {
          case TermBranch(childs) =>
            childs.foldLeft(none[((TermNodeRangeSet, TermNodeRangeSet), Set[String])].success[FatalError]) {
              case (Success(None), child) =>
                checkSuperdisjunctionNode(node, rangeSets, depthRangeSets2)(rangeSetPair)
              case (res, _)               =>
                res
            }
          case TermLeaf(varName) =>
            rangeSets.get(varName).map {
              rs =>
                val (rs1, rs2) = rs.supersetAndSubset(nextDepthRangeSet)
                some(((rangeSetPair._1 & rs1, rangeSetPair._2 & rs2), Set(varName))).success
            }.getOrElse(FatalError("not found narrowest range set", NoPosition).failure)
        }
      case _ :: Nil =>
        FatalError("too few list of depth range sets", NoPosition).failure
      case Nil =>
        FatalError("empty list of depth range sets", NoPosition).failure
    }
  
  private def matchesTermsWithoutVarArgs(term1: MatchingTerm, term2: MatchingTerm, matching: Matching.Value) =
    matching match {
      case Matching.Terms             =>
        for {
          // superterm with term
          // superconjunctions with conjunctions
          optPair1 <- checkSuperconjunctionNode(term1.conjNode, term2.conjRangeSets, term2.conjDepthRangeSets)(fullRangeSetPair)
          // superdisjunctions with disjunctions
          optPair2 <- checkSuperdisjunctionNode(term2.conjNode, term1.disjRangeSets, term1.disjDepthRangeSets)(fullRangeSetPair)
          // term with superterm
          // conjunctions with superconjunctions
          optPair3 <- checkSuperconjunctionNode(term2.conjNode, term1.conjRangeSets, term1.conjDepthRangeSets)(fullRangeSetPair)
          // disjunctions with superdisjunctions
          optPair4 <- checkSuperdisjunctionNode(term1.conjNode, term2.disjRangeSets, term2.disjDepthRangeSets)(fullRangeSetPair)
        } yield  {
          for {
            (_, vns1)  <- optPair1; (_, vns2) <- optPair2
            (_, vns3)  <- optPair1; (_, vns4) <- optPair2
          } yield (vns1 | vns2 | vns3 | vns3)
        }
      case Matching.SupertermWithTerm =>
        for {
          // superconjunctions with conjunctions
          optPair1 <- checkSuperconjunctionNode(term1.conjNode, term2.conjRangeSets, term2.conjDepthRangeSets)(fullRangeSetPair)
          // superdisjunctions with disjunctions
          optPair2 <- checkSuperdisjunctionNode(term2.conjNode, term1.disjRangeSets, term1.disjDepthRangeSets)(fullRangeSetPair)
        } yield (for((_, vns1)  <- optPair1; (_, vns2) <- optPair2) yield (vns1 | vns2))
      case Matching.TermWithSuperterm =>
        for {
          // conjunctions with superconjunctions
          optPair1 <- checkSuperconjunctionNode(term2.conjNode, term1.conjRangeSets, term1.conjDepthRangeSets)(fullRangeSetPair)
          // disjunctions with superdisjunctions
          optPair2 <- checkSuperdisjunctionNode(term1.conjNode, term2.disjRangeSets, term2.disjDepthRangeSets)(fullRangeSetPair)
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