/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.logicalterm.range4
import scala.collection.immutable.IntMap
import scala.collection.immutable.SortedMap
import scala.collection.immutable.Queue
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.logicalterm._

class MatchingTermMatcher extends Matcher[MatchingTerm]
{
  private def termNodeFromTerm(term: Term)(varArgs: Map[String, Vector[Term]]): Option[(Map[String, Vector[Term]], TermNode)] =   
    term.normalizedTerm match {
      case VarApp(name, args) =>
        if(varArgs.get(name).map { _ === args.toVector }.getOrElse(true))
          some((varArgs + (name -> args.toVector), TermLeaf(name)))
        else
          none
      case logicalTerm: LogicalTerm =>
        logicalTerm.terms.foldLeft(some((varArgs, (Vector[TermNode](), 0)))) {
          case (Some((newVarArgs, (termNodes, varCount))), term) =>
            termNodeFromTerm(term)(newVarArgs).map { _.mapElements(identity, n => (termNodes :+ n, varCount + n.varCount)) }
          case (None, _)                          =>
            none
        }.map { _.mapElements(identity, { case (tn, vc) => TermBranch(tn, vc) }) }
    }
  
  override def matchingTermFromTerm(term: Term) =
    (term match {
      case Disjunction(_) => termNodeFromTerm(term)(Map()).map { t => (t._1, TermBranch(Vector(t._2), t._2.varCount)) }
      case _              => termNodeFromTerm(term)(Map())
    }).flatMap {
      case (varArgMap, node) =>
        varArgMap.foldLeft(some(Map[String, Vector[MatchingTerm]]())) {
          case (optVarArgMap2, (varName, varArgs)) => 
            optVarArgMap2.flatMap {
              varArgMap2 =>
                varArgs.foldLeft(some(Vector[MatchingTerm]())) {
                  (ovas, va) => ovas.flatMap { vas => matchingTermFromTerm(va).map { vas :+ _ } }
                }.map { vas => varArgMap2 + (varName -> vas) }
            }
        }.map { MatchingTerm(node, _) }
    }
  
  private def checkOrDistributeSuperconjunctionNode(node: TermNode, rangeSets: Map[String, TermNodeRangeSet], depthRangeSets: List[TermNodeRangeSet], isRoot: Boolean)(varIdx: Int): List[(Option[TermNodeRangeSet], TermNode)] = {
    val depthRangeSet = depthRangeSets.headOption.getOrElse(TermNodeRangeSet.empty)
    (node match {
      case TermBranch(childs, _) =>
        val (_, pairs7) = childs.foldLeft((varIdx, List[(Option[TermNodeRangeSet], TermNode)]())) {
          case ((newVarIdx, pairs), child) =>
            val pairs2 = checkOrDistributeSuperdisjunctionNode(child, rangeSets, depthRangeSets, false)(newVarIdx)
            if(!pairs.isEmpty) {
              val (pairs6, pairIdxs4) = pairs.foldLeft((List[(Option[TermNodeRangeSet], TermNode)](), Set[Int]())) {
                case ((pairs3, pairIdxs), pair @ (optRangeSet, newChild)) =>
                  val (pairs5, pairIdxs3, isIntersected2) = pairs2.zipWithIndex.foldLeft((pairs3, pairIdxs, false)) {
                    case ((pairs4, pairIdxs2, isIntersected), (pair2 @ (optRangeSet2, newChild2), pairIdx)) =>
                      val optRangeSet3 = (optRangeSet |@| optRangeSet2) { _ & _ }
                      if(optRangeSet3.map { rs => !rs.isEmpty }.getOrElse(true))
                        (((optRangeSet3, newChild.withChild(newChild2))) :: pairs4, pairIdxs2 + pairIdx, true)
                      else
                        (pairs4, pairIdxs2, isIntersected)
                  }
                  (if(isIntersected2) pairs5 else (pair :: pairs5), pairIdxs3)
              }
              (newVarIdx + child.varCount, pairs2.zipWithIndex.flatMap { case ((ors, n), pi) => if(!pairIdxs4.contains(pi)) List((ors, TermBranch(Vector(n), n.varCount))) else Nil } ++ pairs6)
            } else
              (newVarIdx + child.varCount, pairs2.map { case (ors, n) => (ors, TermBranch(Vector(n), n.varCount)) })
        }
        if(isRoot)
          pairs7.headOption.map {
            pair =>
              if(pairs7.size > 1)
                List(pairs7.foldLeft((some(TermNodeRangeSet.empty), TermBranch(Vector(), 0))) {
                  case ((_, n), (_, n2)) => (none[TermNodeRangeSet], n &| n2)
                })
              else
                List(pair)
          }.getOrElse(List((some(TermNodeRangeSet.empty), TermBranch(Vector(), 0))))
        else
          pairs7
      case TermLeaf(_) =>
        checkOrDistributeSuperdisjunctionNode(node, rangeSets, depthRangeSets, false)(varIdx)
    }).map { case (ors, n) => (ors.map { _.superset(depthRangeSet) }, n.normalizedTermNode) }
  }

  private def checkOrDistributeSuperdisjunctionNode(node: TermNode, rangeSets: Map[String, TermNodeRangeSet], depthRangeSets: List[TermNodeRangeSet], isRoot: Boolean)(varIdx: Int): List[(Option[TermNodeRangeSet], TermNode)] = {
    val depthRangeSets2 = depthRangeSets.headOption.map { _ => depthRangeSets.tail }.getOrElse(Nil)
    (node match {
      case TermBranch(childs, _) =>
        childs.foldLeft((varIdx, List[(Option[TermNodeRangeSet], TermNode)]())) {
          case ((newVarIdx, pairs), child) =>
            val pairs2 = checkOrDistributeSuperconjunctionNode(child, rangeSets, depthRangeSets2, isRoot)(newVarIdx)
            val pairs5 = if(!pairs.isEmpty)
              pairs.foldLeft(List[(Option[TermNodeRangeSet], TermNode)]()) {
                case (pairs3, pair @ (optRangeSet, newChild)) =>
                  pairs2.foldLeft(pairs3) {
                    case (pairs4, pair2 @ (optRangeSet2, newChild2)) =>
                      val optRangeSet3 = if(!isRoot)
                        (optRangeSet |@| optRangeSet2) { _ | _ }.orElse(optRangeSet).orElse(optRangeSet2)
                      else
                        (optRangeSet |@| optRangeSet2) { _ | _ }
                      ((optRangeSet3, newChild.withChild(newChild2))) :: pairs4 
                  }
              }
            else
              pairs2.map { case (ors, n) => (ors, TermBranch(Vector(n), n.varCount)) }
            (newVarIdx + child.varCount, pairs5)
        }._2
      case TermLeaf(varName) =>
        rangeSets.get(varName).map {
          rs =>
            List((some(depthRangeSets2.headOption.map(rs.withValuesFromVarIndex(varIdx).superset).getOrElse(rs.withValuesFromVarIndex(varIdx))), node))
        }.getOrElse(List((none, node)))
    }).map { case (ors, n) => (ors, n.normalizedTermNode) }
  }
  
  private def checkSuperconjunctionNode(node: TermNode, rangeSets: Map[String, TermNodeRangeSet], depthRangeSets: List[TermNodeRangeSet])(varIdx: Int): TermNodeRangeSet = {
    val depthRangeSet = depthRangeSets.headOption.getOrElse(TermNodeRangeSet.empty)
    (node match {
      case TermBranch(childs, _) =>
        childs.foldLeft((varIdx, TermNodeRangeSet.full)) {
          case ((newVarIdx, rangeSet), child) =>
            (newVarIdx + child.varCount, rangeSet & checkSuperdisjunctionNode(child, rangeSets, depthRangeSets)(newVarIdx))
        }._2
      case TermLeaf(_) =>
        checkSuperdisjunctionNode(node, rangeSets, depthRangeSets)(varIdx)
    }).superset(depthRangeSet)
  }
  
  private def checkSuperdisjunctionNode(node: TermNode, rangeSets: Map[String, TermNodeRangeSet], depthRangeSets: List[TermNodeRangeSet])(varIdx: Int): TermNodeRangeSet = {
    val depthRangeSets2 = depthRangeSets.headOption.map { _ => depthRangeSets.tail }.getOrElse(Nil)
    node match {
      case TermBranch(childs, _) =>
        childs.foldLeft((varIdx, TermNodeRangeSet.empty)) {
          case ((newVarIdx, rangeSet), child) =>
            (newVarIdx + child.varCount, rangeSet | checkSuperconjunctionNode(child, rangeSets, depthRangeSets2)(newVarIdx))
        }._2
      case TermLeaf(varName) =>
        rangeSets.get(varName).map {
          rs =>
            (depthRangeSets2.headOption.map(rs.withValuesFromVarIndex(varIdx).superset).getOrElse(rs.withValuesFromVarIndex(varIdx)))
        }.getOrElse(TermNodeRangeSet.empty)
    }
  }
  
  private def generateCountGraphForConjunction(otherVarIdxs: Map[Int, Set[Int]], node: TermNode, isSuperterm: Boolean)(varIdx: Int)(tuple: (CounterGraph[CounterGraphLocation], Set[(CounterGraphLocation, CounterGraphLocation)], Map[Int, String])): (CounterGraph[CounterGraphLocation], Set[(CounterGraphLocation, CounterGraphLocation)], Map[Int, String]) =
	node match {
	  case TermBranch(childs, _) =>
	    val uLoc = CounterGraphLocation(TermNodeRange(varIdx, varIdx + node.varCount - 1), isSuperterm)
	    val (g2, es2, vns2) = childs.foldLeft((varIdx, tuple)) {
	      case ((newVarIdx, newTuple), child) =>
	        val (newG2, newEs2, newVns2) = generateCountGraphForDisjunction(otherVarIdxs, child, isSuperterm)(newVarIdx)(newTuple)
	        val vLoc = CounterGraphLocation(TermNodeRange(newVarIdx, newVarIdx + child.varCount - 1), isSuperterm)
	        (newVarIdx + child.varCount, (newG2.withTwoEdges(vLoc, uLoc), newEs2, newVns2))
	    }._2
	    (if(childs.size > 1) g2.withCount(uLoc, 1) else g2, es2, vns2)
	  case TermLeaf(varName)     =>
	    generateCountGraphForDisjunction(otherVarIdxs, node, isSuperterm)(varIdx)(tuple)
	}

  private def generateCountGraphForDisjunction(otherVarIdxs: Map[Int, Set[Int]], node: TermNode, isSuperterm: Boolean)(varIdx: Int)(tuple: (CounterGraph[CounterGraphLocation], Set[(CounterGraphLocation, CounterGraphLocation)], Map[Int, String])): (CounterGraph[CounterGraphLocation], Set[(CounterGraphLocation, CounterGraphLocation)], Map[Int, String]) =
	node match {
	  case TermBranch(childs, _) =>
	    val uLoc = CounterGraphLocation(TermNodeRange(varIdx, varIdx + node.varCount - 1), isSuperterm)
	    val (g2, es2, vns2) = childs.foldLeft((varIdx, tuple)) {
	      case ((newVarIdx, newTuple), child) =>
	        val (newG2, newEs2, newVns2) = generateCountGraphForConjunction(otherVarIdxs, child, isSuperterm)(newVarIdx)(newTuple)
	        val vLoc = CounterGraphLocation(TermNodeRange(newVarIdx, newVarIdx + child.varCount - 1), isSuperterm)
	        (newVarIdx + child.varCount, (newG2.withTwoEdges(vLoc, uLoc), newEs2, newVns2))
	    }._2
	    (if(childs.size > 1) g2.withCount(uLoc, childs.size) else g2, es2, vns2)
	  case TermLeaf(varName)     =>
	    val (g, es, vns) = tuple
	    val vLoc = CounterGraphLocation(TermNodeRange(varIdx, varIdx), isSuperterm)
	    if(otherVarIdxs.contains(varIdx)) {
	      val es2 =  otherVarIdxs.get(varIdx).toSet.flatMap { 
	        _.map { i => vLoc -> CounterGraphLocation(TermNodeRange(i, i), !isSuperterm) }
	      }
	      (g.withCount(vLoc, 1), es ++ es2, vns + (varIdx -> varName))
	    } else
	      (g.withCount(vLoc, 0), es, vns)
	}
  
  private def counterGraphWithTwoVarEdgeSets(graph: CounterGraph[CounterGraphLocation], edges1: Set[(CounterGraphLocation, CounterGraphLocation)], edges2: Set[(CounterGraphLocation, CounterGraphLocation)]) = {
    val swappedEdges2 = edges2.map { _.swap }
    val intersectedEdges = edges1 & swappedEdges2
    val otherEdges = (edges1 | swappedEdges2) &~ intersectedEdges
    val otherVLocs = otherEdges.flatMap { p => Set(p._1, p._2) } -- intersectedEdges.flatMap { p => Set(p._1, p._2) }
    val graph2 = intersectedEdges.foldLeft(graph) {
      (newGraph, edge) => newGraph.withTwoEdges(edge._1, edge._2)
    }
    otherVLocs.foldLeft(graph2) { _.withCount(_, 0) }
  }
  
  private def fullyCheckOrDistributeSuperconjunctionNode(node: TermNode, rangeSets: Map[String, TermNodeRangeSet], depthRangeSets: List[TermNodeRangeSet], varArgs: Map[String, Vector[MatchingTerm]]) =
    checkOrDistributeSuperconjunctionNode(node, rangeSets, depthRangeSets, true)(0) match {
      case List((optRangeSet, newNode)) => some((optRangeSet, MatchingTerm(newNode.withIndexes, varArgs)))
      case _                            => none
    }
  
  private def fullyCheckOrDistributeSuperdisjunctionNode(node: TermNode, rangeSets: Map[String, TermNodeRangeSet], depthRangeSets: List[TermNodeRangeSet], varArgs: Map[String, Vector[MatchingTerm]]) =
    checkOrDistributeSuperdisjunctionNode(node, rangeSets, depthRangeSets, true)(0) match {
      case List((optRangeSet, newNode)) => some((optRangeSet, MatchingTerm(newNode.withIndexes, varArgs)))
      case _                            => none
    }
  
  private def checkOrDistributeSuperconjunctionNodeAndDisjunctionNodeFromTerms(term1: MatchingTerm, term2: MatchingTerm, isFirstTry: Boolean) =
    (term1.conjNode, term2.conjNode) match {
      case (TermBranch(childs1, _), TermBranch(childs2, _)) if childs1.size > 1 && childs2.size === 1 && !isFirstTry =>
        val conjDepthRangeSets = TermNodeRangeSet.full :: term2.info.conjDepthRangeSets
        val disjDepthRangeSet = term2.info.conjDepthRangeSets.headOption.getOrElse(TermNodeRangeSet.full)
        val disjDepthRangeSets = TermNodeRangeSet.full :: TermNodeRangeSet.full :: disjDepthRangeSet :: term1.info.conjDepthRangeSets
        for {
          distributedTerm1 <- fullyCheckOrDistributeSuperconjunctionNode(TermBranch(Vector(TermBranch(Vector(term1.conjNode), term1.conjNode.varCount)), term1.conjNode.varCount), term2.info.conjRangeSets, conjDepthRangeSets, term1.varArgs)
          distributedTerm2 <- fullyCheckOrDistributeSuperdisjunctionNode(term2.conjNode, term1.info.disjRangeSets, disjDepthRangeSets, term2.varArgs)
        } yield (distributedTerm1, distributedTerm2)
      case (TermBranch(childs1, _), TermBranch(childs2, _)) if childs1.size === 1 && childs2.size > 1 && isFirstTry =>
        val conjDepthRangeSet = term2.info.conjDepthRangeSets.headOption.getOrElse(TermNodeRangeSet.full)
        val conjDepthRangeSets = TermNodeRangeSet.full :: conjDepthRangeSet :: term2.info.conjDepthRangeSets
        val disjDepthRangeSets = TermNodeRangeSet.full :: TermNodeRangeSet.full :: term1.info.disjDepthRangeSets
        for {
          distributedTerm1 <- fullyCheckOrDistributeSuperconjunctionNode(term1.conjNode, term2.info.conjRangeSets, conjDepthRangeSets, term1.varArgs)
          distributedTerm2 <- fullyCheckOrDistributeSuperdisjunctionNode(TermBranch(Vector(TermBranch(Vector(term2.conjNode), term2.conjNode.varCount)), term2.conjNode.varCount), term1.info.disjRangeSets, disjDepthRangeSets, term2.varArgs)
        } yield (distributedTerm1, distributedTerm2)
      case _ =>
        val conjDepthRangeSets = TermNodeRangeSet.full :: term2.info.conjDepthRangeSets
        val disjDepthRangeSets = TermNodeRangeSet.full :: TermNodeRangeSet.full :: term1.info.disjDepthRangeSets
        for {
          distributedTerm1 <- fullyCheckOrDistributeSuperconjunctionNode(term1.conjNode, term2.info.conjRangeSets, conjDepthRangeSets, term1.varArgs)
          distributedTerm2 <- fullyCheckOrDistributeSuperdisjunctionNode(term2.conjNode, term1.info.disjRangeSets, disjDepthRangeSets, term2.varArgs)
        } yield (distributedTerm1, distributedTerm2)
    }
  
  private def checkSuperconjunctionNodeAndDisjunctionNodeFromTerms(term1: MatchingTerm, term2: MatchingTerm, isFirstTry: Boolean) =
    checkOrDistributeSuperconjunctionNodeAndDisjunctionNodeFromTerms(term1, term2, isFirstTry).map {
      case ((optRangeSet1, distributedTerm1), (optRangeSet2, distributedTerm2)) =>
        val conjDepthRangeSets = TermNodeRangeSet.full :: distributedTerm2.info.conjDepthRangeSets
        val disjDepthRangeSets = TermNodeRangeSet.full :: TermNodeRangeSet.full :: distributedTerm1.info.disjDepthRangeSets
        val optRangeSetPair = (optRangeSet1 |@| optRangeSet2) { (_, _) }
        val conjRangeSet = optRangeSetPair.map { _._1 }.getOrElse {
          checkSuperconjunctionNode(distributedTerm1.conjNode, distributedTerm2.info.conjRangeSets, conjDepthRangeSets)(0)
        }
        val disjRangeSet = optRangeSetPair.map { _._2 }.getOrElse {
          checkSuperdisjunctionNode(distributedTerm2.conjNode, distributedTerm1.info.disjRangeSets, disjDepthRangeSets)(0)
        }
        ((distributedTerm1, conjRangeSet), (distributedTerm2, disjRangeSet))
    }
  
  private def partiallyMatchesSupertermWithTerm(term1: MatchingTerm, term2: MatchingTerm, isFirstTry: Boolean) = {
    checkSuperconjunctionNodeAndDisjunctionNodeFromTerms(term1, term2, isFirstTry).flatMap {
      case ((distributedTerm1, conjRangeSet), (distributedTerm2, disjRangeSet)) =>
        if(!conjRangeSet.isEmpty && !disjRangeSet.isEmpty) {
          val conjOtherVarIdxs = conjRangeSet.value.varIdxPairs.toSet.foldLeft(Map[Int, Set[Int]]()) {
            case (is, (i, j)) => is |+| Map(i -> Set(j))
          }
          val disjOtherVarIdxs = disjRangeSet.value.varIdxPairs.toSet.foldLeft(Map[Int, Set[Int]]()) {
            case (is, (i, j)) => is |+| Map(i -> Set(j))
          }
          val (graph, conjEdges, conjVarNames) = generateCountGraphForConjunction(conjOtherVarIdxs, distributedTerm1.conjNode, true)(0)((CounterGraph.empty, Set(), Map()))
          val (graph2, disjEdges, disjVarNames) = generateCountGraphForDisjunction(disjOtherVarIdxs, distributedTerm2.conjNode, false)(0)((graph, Set(), Map()))
          val graph3 = counterGraphWithTwoVarEdgeSets(graph2, conjEdges, disjEdges)
          graph3.decreaseCounters.flatMap {
            graph4 =>
              val vLoc = CounterGraphLocation(TermNodeRange(0, distributedTerm1.conjNode.varCount - 1), true)
              val uLoc = CounterGraphLocation(TermNodeRange(0, distributedTerm2.conjNode.varCount - 1), false)
              (for(v <- graph4.vertices.get(vLoc); u <- graph4.vertices.get(uLoc)) yield (v, u)).flatMap {
                case (v, u) =>
                  if(v.count > 0 && u.count > 0) {
                    val conjVarNames2 = conjVarNames.flatMap {
                      case (idx, name) =>
                        val tLoc = CounterGraphLocation(TermNodeRange(idx, idx), true)
                        if(graph4.vertices.get(tLoc).map { _.count > 0 }.getOrElse(false)) Set(name) else Set[String]()
                    }
                    val disjVarNames2 = disjVarNames.flatMap {
                      case (idx, name) =>
                        val tLoc = CounterGraphLocation(TermNodeRange(idx, idx), false)
                        if(graph4.vertices.get(tLoc).map { _.count > 0 }.getOrElse(false)) Set(name) else Set[String]()
                    }
                    some(conjVarNames2.toSet | disjVarNames2.toSet)
                  } else
                    none
              }
          }
        } else
          none
    }
  }
    
  private def matchesSupertermWithTerm(term1: MatchingTerm, term2: MatchingTerm) =
    (term1.conjNode, term2.conjNode) match {
      case (TermBranch(childs1, _), TermBranch(childs2, _)) if (childs1.size > 1 && childs2.size === 1) || (childs1.size === 1 && childs2.size > 1) =>
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
