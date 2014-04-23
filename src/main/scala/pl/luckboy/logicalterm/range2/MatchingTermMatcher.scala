/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.logicalterm.range2
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
  
  private def rangeSetsFromConjunctionNode(node: TermNode)(tuple: (Map[String, TermNodeRangeSet], Map[String, TermNodeRangeSet], List[TermNodeRangeSet], List[TermNodeRangeSet])): ((Map[String, TermNodeRangeSet], Map[String, TermNodeRangeSet], List[TermNodeRangeSet], List[TermNodeRangeSet]), Map[String, Set[Int]], Option[TermNodeRange]) = {
    val (conjRangeSets, disjRangeSets, conjDepthRangeSets, disjDepthRangeSets) = tuple
    val (conjDepthRangeSet, nextConjDepthRangeSets) = conjDepthRangeSets.headOption.map {
     (_, conjDepthRangeSets.tail)
    }.getOrElse(TermNodeRangeSet.empty, Nil)
    node match {
      case TermBranch(childs) =>
        val tuple2 = tuple.copy(_3 = nextConjDepthRangeSets)
        val (tuple3, varIdxs, optRange) = childs.foldLeft((tuple2, Map[String, Set[Int]](), none[TermNodeRange])) {
          case ((newTuple, newVarIdxs, optNewRange), child) =>
            val (newTuple2, newVarIdxs2, optNewRange2) = rangeSetsFromDisjunctionNode(child)(newTuple)
            (newTuple2, newVarIdxs |+| newVarIdxs2, (optNewRange |@| optNewRange2) { _ | _ }.orElse(optNewRange2))
        }
        val conjRangeSets2 = tuple3._1
        val conjRangeSets3 = conjRangeSets2 ++ varIdxs.map { 
          case (name, idxs) => 
            val pair = VarIndexSeqPair(ConcatSeq.fromIterable(idxs), ConcatSeq())
            name -> (conjRangeSets2.getOrElse(name, TermNodeRangeSet.empty) | optRange.map { r => TermNodeRangeSet(SortedMap(r -> pair)) }.getOrElse(TermNodeRangeSet.empty))
        }
        val conjDepthRangeSet2 = conjDepthRangeSet | optRange.map { r => TermNodeRangeSet(SortedMap(r -> VarIndexSeqPair.empty)) }.getOrElse(TermNodeRangeSet.empty)
        (tuple3.copy(_1 = conjRangeSets3, _3 = conjDepthRangeSet2 :: tuple3._3), Map(), optRange)
      case TermLeaf(varName, varIdx) =>
        val range = TermNodeRange(varIdx, varIdx)
        val pair = VarIndexSeqPair(ConcatSeq(varIdx), ConcatSeq())
        val conjRangeSets2 = conjRangeSets + (varName -> (conjRangeSets.getOrElse(varName, TermNodeRangeSet.empty) | TermNodeRangeSet(SortedMap(range -> pair))))
        val conjDepthRangeSet2 = conjDepthRangeSet | TermNodeRangeSet(SortedMap(range -> VarIndexSeqPair.empty))
        (tuple.copy(_1 = conjRangeSets2, _3 = conjDepthRangeSet2 :: nextConjDepthRangeSets), Map(varName -> Set(varIdx)), some(TermNodeRange(varIdx, varIdx)))
    }
  }

  private def rangeSetsFromDisjunctionNode(node: TermNode)(tuple: (Map[String, TermNodeRangeSet], Map[String, TermNodeRangeSet], List[TermNodeRangeSet], List[TermNodeRangeSet])): ((Map[String, TermNodeRangeSet], Map[String, TermNodeRangeSet], List[TermNodeRangeSet], List[TermNodeRangeSet]), Map[String, Set[Int]], Option[TermNodeRange]) = {
    val (conjRangeSets, disjRangeSets, conjDepthRangeSets, disjDepthRangeSets) = tuple
    val (disjDepthRangeSet, nextDisjDepthRangeSets) = disjDepthRangeSets.headOption.map {
      (_, disjDepthRangeSets.tail)
    }.getOrElse(TermNodeRangeSet.empty, Nil)
    node match {
      case TermBranch(childs) =>
        val tuple2 = tuple.copy(_4 = nextDisjDepthRangeSets)
        val (tuple3, varIdxs, optRange) = childs.foldLeft((tuple2, Map[String, Set[Int]](), none[TermNodeRange])) {
          case ((newTuple, newVarIdxs, optNewRange), child) =>
            val (newTuple2, newVarIdxs2, optNewRange2) = rangeSetsFromConjunctionNode(child)(newTuple)
            (newTuple2, newVarIdxs |+| newVarIdxs2, (optNewRange |@| optNewRange2) { _ | _ }.orElse(optNewRange2))
        }
        val disjRangeSets2 = tuple3._2
        val disjRangeSets3 = disjRangeSets2 ++ varIdxs.map { 
          case (name, idxs) => 
            val pair = VarIndexSeqPair(ConcatSeq.fromIterable(idxs), ConcatSeq())
            name -> (disjRangeSets2.getOrElse(name, TermNodeRangeSet.empty) | optRange.map { r => TermNodeRangeSet(SortedMap(r -> pair)) }.getOrElse(TermNodeRangeSet.empty))
        }
        val disjDepthRangeSet2 = disjDepthRangeSet | optRange.map { r => TermNodeRangeSet(SortedMap(r -> VarIndexSeqPair.empty)) }.getOrElse(TermNodeRangeSet.empty)
        (tuple3.copy(_2 = disjRangeSets3, _4 = disjDepthRangeSet2 :: tuple3._4), Map(), optRange)
      case TermLeaf(varName, varIdx) =>
        val range = TermNodeRange(varIdx, varIdx)
        val pair = VarIndexSeqPair(ConcatSeq(varIdx), ConcatSeq())
        val disjRangeSets2 = disjRangeSets + (varName -> (disjRangeSets.getOrElse(varName, TermNodeRangeSet.empty) | TermNodeRangeSet(SortedMap(range -> pair))))
        val disjDepthRangeSet2 = disjDepthRangeSet | TermNodeRangeSet(SortedMap(range -> VarIndexSeqPair.empty))
        (tuple.copy(_2 = disjRangeSets2, _4 = disjDepthRangeSet2 :: nextDisjDepthRangeSets), Map(varName -> Set(varIdx)), some(TermNodeRange(varIdx, varIdx)))
    }
  }
  
  private def rangeSetsFromTermNode(node: TermNode)(tuple: (Map[String, TermNodeRangeSet], Map[String, TermNodeRangeSet], List[TermNodeRangeSet], List[TermNodeRangeSet])) = {
    val (tuple, varIdxs, optRange) = rangeSetsFromConjunctionNode(node)((Map(), Map(), Nil, Nil))
    val (conjRangeSets, disjRangeSets, conjDepthRangeSets, disjDepthRangeSets) = tuple
    val disjRangeSets2 = tuple._2 ++ varIdxs.map { 
      case (name, idxs) => 
        val pair = VarIndexSeqPair(ConcatSeq.fromIterable(idxs), ConcatSeq())
        name -> (disjRangeSets.getOrElse(name, TermNodeRangeSet.empty) | optRange.map { r => TermNodeRangeSet(SortedMap(r -> pair)) }.getOrElse(TermNodeRangeSet.empty))
    }
    tuple.copy(_2 = disjRangeSets2)
  }
  
  private def matchingTermFromTermNodeAndVarArgs(node: TermNode, varArgs: Map[String, Vector[MatchingTerm]]) = {
    val (conjRangeSet, disjRangeSet, conjDepthRangeSets, disjDepthRangeSets) = rangeSetsFromTermNode(node)((Map(), Map(), Nil, Nil))
    MatchingTerm(node, conjRangeSet, disjRangeSet, conjDepthRangeSets, disjDepthRangeSets, varArgs)
  }
  
  override def matchingTermFromTerm(term: Term) =
    (term match {
      case Disjunction(_) => termNodeFromTerm(term)((Map(), 0)).map { t => (t._1, TermBranch(Vector(t._2))) }
      case _              => termNodeFromTerm(term)((Map(), 0))
    }).flatMap {
      case ((varArgMap, _), node) =>
        varArgMap.foldLeft(some(Map[String, Vector[MatchingTerm]]())) {
          case (optVarArgMap2, (varName, varArgs)) => 
            optVarArgMap2.flatMap {
              varArgMap2 =>
                varArgs.foldLeft(some(Vector[MatchingTerm]())) {
                  (ovas, va) => ovas.flatMap { vas => matchingTermFromTerm(va).map { vas :+ _ } }
                }.map { vas => varArgMap2 + (varName -> vas) }
            }
        }.map { matchingTermFromTermNodeAndVarArgs(node, _) }
    }
  
  private def distributeSuperconjunctionNode(node: TermNode, rangeSets: Map[String, TermNodeRangeSet], depthRangeSets: List[TermNodeRangeSet], isRoot: Boolean): List[(Option[TermNodeRangeSet], TermNode)] = {
    val depthRangeSet = depthRangeSets.headOption.getOrElse(TermNodeRangeSet.empty)
    (node match {
      case TermBranch(childs) =>
        val pairs6 = childs.foldLeft(List[(Option[TermNodeRangeSet], TermNode)]()) {
          (pairs, child) =>
            val pairs2 = distributeSuperdisjunctionNode(child, rangeSets, depthRangeSets, false)
            if(!pairs.isEmpty) {
              val (pairs5, pairIdxs3) = pairs.foldLeft((List[(Option[TermNodeRangeSet], TermNode)](), Set[Int]())) {
                case ((pairs3, pairIdxs), pair @ (optRangeSet, newChild)) =>
                  pairs2.zipWithIndex.foldLeft((pairs3, pairIdxs)) {
                    case ((pairs4, pairIdxs2), (pair2 @ (optRangeSet2, newChild2), pairIdx)) =>
                      val optRangeSet3 = (optRangeSet |@| optRangeSet2) { _ & _ }
                      if(optRangeSet3.map { rs => !rs.isEmpty }.getOrElse(true))
                        (((optRangeSet3, newChild.withChild(newChild2))) :: pairs4, pairIdxs2 + pairIdx)
                      else
                        ((pair :: pairs4), pairIdxs2)
                  }
              }
              pairs2.zipWithIndex.flatMap { case ((ors, n), pi) => if(!pairIdxs3.contains(pi)) List((ors, TermBranch(Vector(n)))) else Nil } ++ pairs5
            } else
              pairs2.map { case (ors, n) => (ors, TermBranch(Vector(n))) }
        }
        if(isRoot)
          List(pairs6.foldLeft((some(TermNodeRangeSet.empty), TermBranch(Vector()))) {
            case ((ors, n), (ors2, n2)) => ((ors |@| ors2) { _ & _ }, n &| n2)
          })
        else
          pairs6
      case TermLeaf(_, _) =>
        distributeSuperdisjunctionNode(node, rangeSets, depthRangeSets, false)
    }).map { case (ors, n) => (ors.map { _.superset(depthRangeSet) }, n.normalizedTermNode) }
  }

  private def distributeSuperdisjunctionNode(node: TermNode, rangeSets: Map[String, TermNodeRangeSet], depthRangeSets: List[TermNodeRangeSet], isRoot: Boolean): List[(Option[TermNodeRangeSet], TermNode)] = {
    val depthRangeSets2 = depthRangeSets.headOption.map { _ => depthRangeSets.tail }.getOrElse(Nil)
    (node match {
      case TermBranch(childs) =>
        childs.foldLeft(List[(Option[TermNodeRangeSet], TermNode)]()) {
          (pairs, child) =>
            val pairs2 = distributeSuperconjunctionNode(child, rangeSets, depthRangeSets2, isRoot)
            if(!pairs.isEmpty)
              pairs.foldLeft(List[(Option[TermNodeRangeSet], TermNode)]()) {
                case (pairs3, pair @ (optRangeSet, newChild)) =>
                  pairs2.foldLeft(pairs3) {
                    case (pairs4, pair2 @ (optRangeSet2, newChild2)) =>
                      val optRangeSet3 = (optRangeSet |@| optRangeSet2) { _ | _ }
                      ((optRangeSet3, newChild.withChild(newChild2))) :: pairs4 
                  }
              }
            else
              pairs2.map { case (ors, n) => (ors, TermBranch(Vector(n))) }
        }
      case TermLeaf(varName, _) =>
        rangeSets.get(varName).map {
          rs => List((some(depthRangeSets2.headOption.map(rs.superset).getOrElse(rs)), node))
        }.getOrElse(List((none, node)))
    }).map { case (ors, n) => (ors, n.normalizedTermNode) }
  }
  
  private def checkSuperconjunctionNode(node: TermNode, rangeSets: Map[String, TermNodeRangeSet], depthRangeSets: List[TermNodeRangeSet]): TermNodeRangeSet = {
    val depthRangeSet = depthRangeSets.headOption.getOrElse(TermNodeRangeSet.empty)
    (node match {
      case TermBranch(childs) =>
        childs.foldLeft(TermNodeRangeSet.full) { _ & checkSuperdisjunctionNode(_, rangeSets, depthRangeSets) }
      case TermLeaf(_, _) =>
        checkSuperdisjunctionNode(node, rangeSets, depthRangeSets)
    }).superset(depthRangeSet)
  }
  
  private def checkSuperdisjunctionNode(node: TermNode, rangeSets: Map[String, TermNodeRangeSet], depthRangeSets: List[TermNodeRangeSet]): TermNodeRangeSet = {
    val depthRangeSets2 = depthRangeSets.headOption.map { _ => depthRangeSets.tail }.getOrElse(Nil)
    node match {
      case TermBranch(childs) =>
        childs.foldLeft(TermNodeRangeSet.empty) { _ | checkSuperconjunctionNode(_, rangeSets, depthRangeSets2) }
      case TermLeaf(varName, varIdx) =>
        rangeSets.get(varName).map {
          rs =>
            (depthRangeSets2.headOption.map(rs.swapPairsWithMyVarIndex(varIdx).superset).getOrElse(rs.swapPairsWithMyVarIndex(varIdx)))
        }.getOrElse(TermNodeRangeSet.empty)
    }
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
  
  private def distributedTermFromSuperconjunctionNode(node: TermNode, rangeSets: Map[String, TermNodeRangeSet], depthRangeSets: List[TermNodeRangeSet], varArgs: Map[String, Vector[MatchingTerm]]) =
    distributeSuperconjunctionNode(node, rangeSets, depthRangeSets, true) match {
      case List((_, newNode)) => some(matchingTermFromTermNodeAndVarArgs(newNode.withIndexes, varArgs))
      case pairs              => none
    }
  
  private def distributedTermFromSuperdisjunctionNode(node: TermNode, rangeSets: Map[String, TermNodeRangeSet], depthRangeSets: List[TermNodeRangeSet], varArgs: Map[String, Vector[MatchingTerm]]) =
    distributeSuperdisjunctionNode(node, rangeSets, depthRangeSets, true) match {
      case List((_, newNode)) => some(matchingTermFromTermNodeAndVarArgs(newNode.withIndexes, varArgs))
      case pairs              => none
    }
  
  private def distributeSuperconjunctionNodeAndDisjunctionNodeFromTerms(term1: MatchingTerm, term2: MatchingTerm, isFirstTry: Boolean) =
    (term1.conjNode, term2.conjNode) match {
      case (TermBranch(childs1), TermBranch(childs2)) if childs1.size > 1 && childs2.size === 1 && !isFirstTry =>
        val conjDepthRangeSets = TermNodeRangeSet.full :: term2.conjDepthRangeSets
        val disjDepthRangeSet = term2.conjDepthRangeSets.headOption.getOrElse(TermNodeRangeSet.full)
        val disjDepthRangeSets = TermNodeRangeSet.full :: TermNodeRangeSet.full :: disjDepthRangeSet :: term1.conjDepthRangeSets
        for {
          distributedTerm1 <- distributedTermFromSuperconjunctionNode(TermBranch(Vector(TermBranch(Vector(term1.conjNode)))), term2.conjRangeSets, conjDepthRangeSets, term1.varArgs)
          distributedTerm2 <- distributedTermFromSuperdisjunctionNode(term2.conjNode, term1.disjRangeSets, disjDepthRangeSets, term2.varArgs)
        } yield (distributedTerm1, distributedTerm2)
      case (TermBranch(childs1), TermBranch(childs2)) if childs1.size === 1 && childs2.size > 1 && isFirstTry =>
        val conjDepthRangeSet = term2.conjDepthRangeSets.headOption.getOrElse(TermNodeRangeSet.full)
        val conjDepthRangeSets = TermNodeRangeSet.full :: conjDepthRangeSet :: term2.conjDepthRangeSets
        val disjDepthRangeSets = TermNodeRangeSet.full :: TermNodeRangeSet.full :: term1.disjDepthRangeSets
        for {
          distributedTerm1 <- distributedTermFromSuperconjunctionNode(term1.conjNode, term2.conjRangeSets, conjDepthRangeSets, term1.varArgs)
          distributedTerm2 <- distributedTermFromSuperdisjunctionNode(TermBranch(Vector(TermBranch(Vector(term2.conjNode)))), term1.disjRangeSets, disjDepthRangeSets, term2.varArgs)
        } yield (distributedTerm1, distributedTerm2)
      case _ =>
        val conjDepthRangeSets = TermNodeRangeSet.full :: term2.conjDepthRangeSets
        val disjDepthRangeSets = TermNodeRangeSet.full :: TermNodeRangeSet.full :: term1.disjDepthRangeSets
        for {
          distributedTerm1 <- distributedTermFromSuperconjunctionNode(term1.conjNode, term2.conjRangeSets, conjDepthRangeSets, term1.varArgs)
          distributedTerm2 <- distributedTermFromSuperdisjunctionNode(term2.conjNode, term1.disjRangeSets, disjDepthRangeSets, term2.varArgs)
        } yield (distributedTerm1, distributedTerm2)
    }
  
  private def checkSuperconjunctionNodeAndDisjunctionNodeFromTerms(term1: MatchingTerm, term2: MatchingTerm, isFirstTry: Boolean) =
    distributeSuperconjunctionNodeAndDisjunctionNodeFromTerms(term1, term2, isFirstTry).map {
      case (distributedTerm1, distributedTerm2) =>
        val conjDepthRangeSets = TermNodeRangeSet.full :: distributedTerm2.conjDepthRangeSets
        val disjDepthRangeSets = TermNodeRangeSet.full :: TermNodeRangeSet.full :: distributedTerm1.disjDepthRangeSets
        val conjRangeSet = checkSuperconjunctionNode(distributedTerm1.conjNode, distributedTerm2.conjRangeSets, conjDepthRangeSets)
        val disjRangeSet = checkSuperdisjunctionNode(distributedTerm2.conjNode, distributedTerm1.disjRangeSets, disjDepthRangeSets)
        ((distributedTerm1, conjRangeSet), (distributedTerm2, disjRangeSet))
    }
  
  private def partiallyMatchesSupertermWithTerm(term1: MatchingTerm, term2: MatchingTerm, isFirstTry: Boolean) = {
    checkSuperconjunctionNodeAndDisjunctionNodeFromTerms(term1, term2, isFirstTry).flatMap {
      case ((distributedTerm1, conjRangeSet), (distributedTerm2, disjRangeSet)) =>
        if(!conjRangeSet.isEmpty && !disjRangeSet.isEmpty) {
          val conjMyVarIdxs = conjRangeSet.varIndexSeqPair.myVarIdxs.toSet
          val disjOtherVarIdxs = disjRangeSet.varIndexSeqPair.otherVarIdxs.toSet
          val disjMyVarIdxs = disjRangeSet.varIndexSeqPair.myVarIdxs.toSet
          val conjOtherVarIdxs = conjRangeSet.varIndexSeqPair.otherVarIdxs.toSet
          for {
            conjVarNames <- checkVarIndexSetsForConjunction(conjMyVarIdxs, disjOtherVarIdxs, distributedTerm1.conjNode)(Set())
            disjVarNames <- checkVarIndexSetsForDisjunction(disjMyVarIdxs, conjOtherVarIdxs, distributedTerm2.conjNode)(Set())
          } yield (conjVarNames | disjVarNames)
        } else
          none
    }
  }
    
  private def matchesSupertermWithTerm(term1: MatchingTerm, term2: MatchingTerm) =
    (term1.conjNode, term2.conjNode) match {
      case (TermBranch(childs1), TermBranch(childs2)) if (childs1.size > 1 && childs2.size === 1) || (childs1.size === 1 && childs2.size > 1) =>
        partiallyMatchesSupertermWithTerm(term1, term2, true).orElse(partiallyMatchesSupertermWithTerm(term1, term2, false))
      case _ =>
        partiallyMatchesSupertermWithTerm(term1, term2, true)
    }
  
  private def matchesTermsWithoutVarArgs(term1: MatchingTerm, term2: MatchingTerm, matching: Matching.Value) =
    matching match {
      case Matching.Terms             =>
        val optVarNames1 = matchesSupertermWithTerm(term1, term2)
        val optVarNames2 = matchesSupertermWithTerm(term2, term1)
        for(varNames1 <- optVarNames1; varNames2 <-optVarNames2) yield (varNames1 | varNames2)
      case Matching.SupertermWithTerm =>
        matchesSupertermWithTerm(term1, term2)
      case Matching.TermWithSuperterm =>
        matchesSupertermWithTerm(term2, term1)
    }      
    
  override def matches(term1: MatchingTerm, term2: MatchingTerm, matching: Matching.Value): Validation[FatalError, Boolean] =
    matchesTermsWithoutVarArgs(term1, term2, matching).map {
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
