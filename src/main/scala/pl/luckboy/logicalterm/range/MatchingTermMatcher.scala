package pl.luckboy.logicalterm.range
import scala.collection.immutable.IntMap
import scala.collection.immutable.SortedMap
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.logicalterm._

class MatchingTermMatcher extends Matcher[MatchingTerm]
{
  private def matchingTermNodeFromTerm(term: Term)(pair: (Int, Map[String, Vector[Term]])): Option[((Int, Map[String, Vector[Term]]), TermNode)] = {
    val (varIdx, varArgs) = pair
    term.normalizedTerm match {
      case VarApp(name, args) =>
        if(varArgs.get(name).map { _ === args.toVector }.getOrElse(true))
          some(((varIdx + 1, varArgs + (name -> args.toVector)), TermLeaf(name, varIdx)))
        else
          none
      case logicalTerm: LogicalTerm =>
        logicalTerm.terms.foldLeft(some((pair, Vector[TermNode]()))) {
          case (Some((newPair, termNodes)), term) =>
            matchingTermNodeFromTerm(term)(newPair).map { _.mapElements(identity, termNodes :+ _) }
          case (None, _)                          =>
            none
        }.map { _.mapElements(identity, TermBranch(_)) }
    }
  }
  
  override def matchingTermFromTerm(term: Term): Option[MatchingTerm] =
    throw new UnsupportedOperationException
  
  private def checkSuperconjunctionNode(node: TermNode, rangeSets: Map[String, TermNodeRangeSet], depthRangeSets: List[TermNodeRangeSet])(rangeSet: TermNodeRangeSet): Validation[FatalError, TermNodeRangeSet] = {
    depthRangeSets match {
      case depthRangeSet :: nextDepthRangeSet :: _ =>
        (node match {
          case TermBranch(childs) =>
            childs.foldLeft(rangeSet.success[FatalError]) {
              case (Success(newRangeSet), child) =>
                checkSuperdisjunctionNode(child, rangeSets, depthRangeSets)(newRangeSet).map { newRangeSet & _ }
              case (res, _)                      =>
                res
            }
          case TermLeaf(_, _) =>
            checkSuperdisjunctionNode(node, rangeSets, depthRangeSets)(rangeSet)
        }).map { _.superset(depthRangeSet) }
      case _ :: Nil =>
        FatalError("too few list of depth range sets", NoPosition).failure
      case Nil =>
        FatalError("empty list of depth range sets", NoPosition).failure
    }
  }
  
  private def checkSuperdisjunctionNode(node: TermNode, rangeSets: Map[String, TermNodeRangeSet], depthRangeSets: List[TermNodeRangeSet])(rangeSet: TermNodeRangeSet): Validation[FatalError, TermNodeRangeSet] =
    depthRangeSets match {
      case _ :: (depthRangeSets2 @ (nextDepthRangeSet :: _)) =>
        node match {
          case TermBranch(childs) =>
            childs.foldLeft(TermNodeRangeSet.empty.success[FatalError]) {
              case (Success(newRangeSet), child) =>
                checkSuperdisjunctionNode(node, rangeSets, depthRangeSets2)(rangeSet).map { newRangeSet | _ }
              case (res, _)                      =>
                res
            }
          case TermLeaf(varName, varIdx) =>
            rangeSets.get(varName).map {
              rs =>
                (rangeSet & rs.swapPairsWithMyVarIndex(varIdx).superset(nextDepthRangeSet)).success
            }.getOrElse(FatalError("not found narrowest range set", NoPosition).failure)
        }
      case _ :: Nil =>
        FatalError("too few list of depth range sets", NoPosition).failure
      case Nil =>
        FatalError("empty list of depth range sets", NoPosition).failure
    }
  
  private def checkVarIndexSetsForConjunction(myVarIdxs: Set[Int], otherVarIdxSet: Set[Int], node: TermNode)(varNames: Set[String]): Option[Set[String]] =
    node match {
      case TermBranch(childs) =>
        childs.foldLeft(some(varNames)) {
          case (Some(varNames2), child) =>
            checkVarIndexSetsForDisjunction(myVarIdxs, otherVarIdxSet, child)(varNames2)
          case (None, _)                =>
            none
        }
      case TermLeaf(varName, varIdx) =>
        checkVarIndexSetsForDisjunction(myVarIdxs, otherVarIdxSet, node)(varNames)
    }

  private def checkVarIndexSetsForDisjunction(myVarIdxs: Set[Int], otherVarIdxSet: Set[Int], node: TermNode)(varNames: Set[String]): Option[Set[String]] =
    node match {
      case TermBranch(childs) =>
        childs.foldLeft(none[Set[String]]) {
          case (None, child)    =>
            checkVarIndexSetsForConjunction(myVarIdxs, otherVarIdxSet, child)(varNames)
          case (optVarNames, _) =>
            optVarNames
        }
      case TermLeaf(varName, varIdx) =>
        if(myVarIdxs.contains(varIdx) && otherVarIdxSet.contains(varIdx)) some(varNames + varName) else none
    }
  
  private def matchesSupertermWithTerm(term1: MatchingTerm, term2: MatchingTerm) =
    for {
      conjRangeSet <- checkSuperconjunctionNode(term1.conjNode, term2.conjRangeSets, term2.conjDepthRangeSets)(TermNodeRangeSet.full)
      disjRangeSet <- checkSuperdisjunctionNode(term2.conjNode, term1.disjRangeSets, term1.disjDepthRangeSets)(TermNodeRangeSet.full)
    } yield {
      if(!conjRangeSet.isEmpty && !disjRangeSet.isEmpty) {
        val conjMyVarIdxs = conjRangeSet.varIndexSeqPair.myVarIdxs.toSet
        val disjOtherVarIdxs = disjRangeSet.varIndexSeqPair.otherVarIdxs.toSet
        val disjMyVarIdxs = disjRangeSet.varIndexSeqPair.myVarIdxs.toSet
        val conjOtherVarIdxs = conjRangeSet.varIndexSeqPair.otherVarIdxs.toSet
        for {
          conjVarNames <- checkVarIndexSetsForConjunction(conjMyVarIdxs, disjOtherVarIdxs, term1.conjNode)(Set())
          disjVarNames <- checkVarIndexSetsForDisjunction(disjMyVarIdxs, conjOtherVarIdxs, term2.conjNode)(Set())
        } yield (conjVarNames | disjVarNames)
      } else
        none
    }
  
  private def matchesTermsWithoutVarArgs(term1: MatchingTerm, term2: MatchingTerm, matching: Matching.Value) =
    matching match {
      case Matching.Terms             =>
        for {
          optVarNames1 <- matchesSupertermWithTerm(term1, term2)
          optVarNames2 <- matchesSupertermWithTerm(term2, term1)
        } yield (for(varNames1 <- optVarNames1; varNames2 <-optVarNames2) yield (varNames1 | varNames2))
      case Matching.SupertermWithTerm =>
        matchesSupertermWithTerm(term1, term2)
      case Matching.TermWithSuperterm =>
        matchesSupertermWithTerm(term2, term1)
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