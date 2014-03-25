package pl.luckboy.logicalterm.range
import scala.collection.immutable.IntMap
import scala.collection.immutable.SortedMap
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.logicalterm._

class MatchingTermMatcher extends Matcher[MatchingTerm]
{
  private def termNodeFromTerm(term: Term)(tuple: (Map[String, Vector[Term]], Int)): Option[((Map[String, Vector[Term]], Int), TermNode)] =   
    term.normalizedTerm match {
      case VarApp(name, args) =>
        val (varArgs, varIdx) = tuple
        if(varArgs.get(name).map { _ === args.toVector }.getOrElse(true))
          some(((varArgs + (name -> args.toVector), varIdx + 1), TermLeaf(name, varIdx)))
        else
          none
      case logicalTerm: LogicalTerm =>
        logicalTerm.terms.foldLeft(some((tuple, Vector[TermNode]()))) {
          case (Some((newTuple, termNodes)), term) =>
            termNodeFromTerm(term)(newTuple).map { _.mapElements(identity, termNodes :+ _) }
          case (None, _)                          =>
            none
        }.map { _.mapElements(identity, TermBranch(_)) }
    }
  
  private def rangeSetsFromConjunctionNode(node: TermNode)(tuple: (Map[String, TermNodeRangeSet], Map[String, TermNodeRangeSet], List[TermNodeRangeSet], List[TermNodeRangeSet])): ((Map[String, TermNodeRangeSet], Map[String, TermNodeRangeSet], List[TermNodeRangeSet], List[TermNodeRangeSet]), Map[String, Set[Int]], Map[String, Set[Int]], Option[TermNodeRange]) =
    node match {
      case TermBranch(childs) =>
        val (conjRangeSets, disjRangeSets, conjDepthRangeSets, disjDepthRangeSets) = tuple
        val (conjDepthRangeSet, nextConjDepthRangeSets) = conjDepthRangeSets.headOption.map {
          (_, conjDepthRangeSets.tail)
        }.getOrElse(TermNodeRangeSet.empty, Nil)
        val tuple2 = tuple.copy(_3 = nextConjDepthRangeSets)
        val (tuple3, conjVarIdxs, disjVarIdxs, optRange) = childs.foldLeft((tuple2, Map[String, Set[Int]](), Map[String, Set[Int]](), none[TermNodeRange])) {
          case ((newTuple, newConjVarIdxs, newDisjVarIdxs, optNewRange), child) =>
            val (newTuple2, newConjVarIdxs2, newDisjVarIdxs2, optNewRange2) = rangeSetsFromDisjunctionNode(child)(newTuple)
            (newTuple2, newConjVarIdxs |+| newConjVarIdxs2, newDisjVarIdxs |+| newDisjVarIdxs2, (optNewRange |@| optNewRange2) { _ | _ }.orElse(optNewRange2))
        }
        val conjRangeSets2 = tuple3._1 ++ conjVarIdxs.map { 
          case (name, idxs) => 
            val pair = VarIndexSeqPair(ConcatSeq.fromIterable(idxs), ConcatSeq())
            name -> (conjRangeSets.getOrElse(name, TermNodeRangeSet.empty) | optRange.map { r => TermNodeRangeSet(SortedMap(r -> pair)) }.getOrElse(TermNodeRangeSet.empty))
        }
        val conjDepthRangeSet2 = conjDepthRangeSet | optRange.map { r => TermNodeRangeSet(SortedMap(r -> VarIndexSeqPair.empty)) }.getOrElse(TermNodeRangeSet.empty)
        (tuple3.copy(_1 = conjRangeSets2, _3 = conjDepthRangeSet2 :: tuple3._3), Map(), disjVarIdxs, optRange)
      case TermLeaf(varName, varIdx) =>
        (tuple, Map(varName -> Set(varIdx)), Map(varName -> Set(varIdx)), some(TermNodeRange(varIdx, varIdx)))
    }

  private def rangeSetsFromDisjunctionNode(node: TermNode)(tuple: (Map[String, TermNodeRangeSet], Map[String, TermNodeRangeSet], List[TermNodeRangeSet], List[TermNodeRangeSet])): ((Map[String, TermNodeRangeSet], Map[String, TermNodeRangeSet], List[TermNodeRangeSet], List[TermNodeRangeSet]), Map[String, Set[Int]], Map[String, Set[Int]], Option[TermNodeRange]) =
    node match {
      case TermBranch(childs) =>
        val (conjRangeSets, disjRangeSets, conjDepthRangeSets, disjDepthRangeSets) = tuple
        val (disjDepthRangeSet, nextDisjDepthRangeSets) = disjDepthRangeSets.headOption.map {
          (_, disjDepthRangeSets.tail)
        }.getOrElse(TermNodeRangeSet.empty, Nil)
        val tuple2 = tuple.copy(_4 = nextDisjDepthRangeSets)
        val (tuple3, conjVarIdxs, disjVarIdxs, optRange) = childs.foldLeft((tuple2, Map[String, Set[Int]](), Map[String, Set[Int]](), none[TermNodeRange])) {
          case ((newTuple, newConjVarIdxs, newDisjVarIdxs, optNewRange), child) =>
            val (newTuple2, newConjVarIdxs2, newDisjVarIdxs2, optNewRange2) = rangeSetsFromConjunctionNode(child)(newTuple)
            (newTuple2, newConjVarIdxs |+| newConjVarIdxs2, newDisjVarIdxs |+| newDisjVarIdxs2, (optNewRange |@| optNewRange2) { _ | _ }.orElse(optNewRange2))
        }
        val disjRangeSets2 = tuple3._2 ++ disjVarIdxs.map { 
          case (name, idxs) => 
            val pair = VarIndexSeqPair(ConcatSeq.fromIterable(idxs), ConcatSeq())
            name -> (disjRangeSets.getOrElse(name, TermNodeRangeSet.empty) | optRange.map { r => TermNodeRangeSet(SortedMap(r -> pair)) }.getOrElse(TermNodeRangeSet.empty))
        }
        val disjDepthRangeSet2 = disjDepthRangeSet | optRange.map { r => TermNodeRangeSet(SortedMap(r -> VarIndexSeqPair.empty)) }.getOrElse(TermNodeRangeSet.empty)
        (tuple3.copy(_2 = disjRangeSets2, _4 = disjDepthRangeSet2 :: tuple3._4), conjVarIdxs, Map(), optRange)
      case TermLeaf(varName, varIdx) =>
        (tuple, Map(varName -> Set(varIdx)), Map(varName -> Set(varIdx)), some(TermNodeRange(varIdx, varIdx)))
    }
  
  private def rangeSetsFromTermNode(node: TermNode)(tuple: (Map[String, TermNodeRangeSet], Map[String, TermNodeRangeSet], List[TermNodeRangeSet], List[TermNodeRangeSet])) = {
    val (tuple, conjVarIdxs, disjVarIdxs, optRange) = rangeSetsFromConjunctionNode(node)((Map(), Map(), Nil, Nil))
    val (conjRangeSets, disjRangeSets, conjDepthRangeSets, disjDepthRangeSets) = tuple
    val conjRangeSets2 = tuple._1 ++ conjVarIdxs.map { 
      case (name, idxs) => 
        val pair = VarIndexSeqPair(ConcatSeq.fromIterable(idxs), ConcatSeq())
        name -> (conjRangeSets.getOrElse(name, TermNodeRangeSet.empty) | optRange.map { r => TermNodeRangeSet(SortedMap(r -> pair)) }.getOrElse(TermNodeRangeSet.empty))
    }
    val disjRangeSets2 = tuple._2 ++ disjVarIdxs.map { 
      case (name, idxs) => 
        val pair = VarIndexSeqPair(ConcatSeq.fromIterable(idxs), ConcatSeq())
        name -> (disjRangeSets.getOrElse(name, TermNodeRangeSet.empty) | optRange.map { r => TermNodeRangeSet(SortedMap(r -> pair)) }.getOrElse(TermNodeRangeSet.empty))
    }
    tuple.copy(_1 = conjRangeSets2, _2 = disjRangeSets2)
  }
  
  override def matchingTermFromTerm(term: Term) =
    (term match {
      case Disjunction(_) => termNodeFromTerm(term)((Map(), 0)).map { t => (t._1, TermBranch(Vector(t._2))) }
      case _              => termNodeFromTerm(term)((Map(), 0))
    }).flatMap {
      case ((varArgMap, _), node) =>
        val (conjRangeSet, disjRangeSet, conjDepthRangeSets, disjDepthRangeSets) = rangeSetsFromTermNode(node)((Map(), Map(), Nil, Nil))
        varArgMap.foldLeft(some(Map[String, Vector[MatchingTerm]]())) {
          case (optVarArgMap2, (varName, varArgs)) => 
            optVarArgMap2.flatMap {
              varArgMap2 =>
                varArgs.foldLeft(some(Vector[MatchingTerm]())) {
                  (ovas, va) => ovas.flatMap { vas => matchingTermFromTerm(va).map { vas :+ _ } }
                }.map { vas => varArgMap2 + (varName -> vas) }
            }
        }.map {
          MatchingTerm(node, conjRangeSet, disjRangeSet, conjDepthRangeSets, disjDepthRangeSets, _)
        }
    }
  
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
  
  private def checkSuperconjunctionNodeFromTerms(term1: MatchingTerm, term2: MatchingTerm) =
    (term1.conjNode, term2.conjNode) match {
      case (TermBranch(childs1), TermBranch(childs2)) if childs1.size === 1 && childs2.size > 1 =>
        term2.conjDepthRangeSets.headOption.map {
          conjDepthRangeSet =>
            val conjDepthRangeSets = TermNodeRangeSet.full :: conjDepthRangeSet :: term2.conjDepthRangeSets
          checkSuperconjunctionNode(term1.conjNode, term2.conjRangeSets, conjDepthRangeSets)(TermNodeRangeSet.full)
        }.getOrElse(FatalError("empty list of depth range sets", NoPosition).failure)
      case _ =>
        val conjDepthRangeSets = TermNodeRangeSet.full :: term2.conjDepthRangeSets
        checkSuperconjunctionNode(term1.conjNode, term2.conjRangeSets, conjDepthRangeSets)(TermNodeRangeSet.full)
    }
  
  private def checkSuperdisjunctionNodeFromTerms(term1: MatchingTerm, term2: MatchingTerm) =
    (term1.conjNode, term2.conjNode) match {
      case (TermBranch(childs1), TermBranch(childs2)) if childs1.size === 1 && childs2.size > 1 =>
        term2.disjDepthRangeSets.headOption.map {
          disjDepthRangeSet =>
            val disjDepthRangeSets = TermNodeRangeSet.full :: disjDepthRangeSet :: term2.disjDepthRangeSets
          checkSuperdisjunctionNode(term1.conjNode, term2.disjRangeSets, disjDepthRangeSets)(TermNodeRangeSet.full)
        }.getOrElse(FatalError("empty list of depth range sets", NoPosition).failure)
      case _ =>
        val disjDepthRangeSets = TermNodeRangeSet.full :: term2.disjDepthRangeSets
        checkSuperdisjunctionNode(term1.conjNode, term2.conjRangeSets, disjDepthRangeSets)(TermNodeRangeSet.full)
    }
  
  private def matchesSupertermWithTerm(term1: MatchingTerm, term2: MatchingTerm) =
    for {
      conjRangeSet <- checkSuperconjunctionNodeFromTerms(term1, term2)
      disjRangeSet <- checkSuperdisjunctionNodeFromTerms(term2, term1)
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