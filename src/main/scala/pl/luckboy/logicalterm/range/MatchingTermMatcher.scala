/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
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
            val pair = TermNodeRangeValue(ConcatSeq.fromIterable(idxs), ConcatSeq())
            name -> (conjRangeSets2.getOrElse(name, TermNodeRangeSet.empty) | optRange.map { r => TermNodeRangeSet(SortedMap(r -> pair)) }.getOrElse(TermNodeRangeSet.empty))
        }
        val conjDepthRangeSet2 = conjDepthRangeSet | optRange.map { r => TermNodeRangeSet(SortedMap(r -> TermNodeRangeValue.empty)) }.getOrElse(TermNodeRangeSet.empty)
        (tuple3.copy(_1 = conjRangeSets3, _3 = conjDepthRangeSet2 :: tuple3._3), Map(), optRange)
      case TermLeaf(varName, varIdx) =>
        val range = TermNodeRange(varIdx, varIdx)
        val pair = TermNodeRangeValue(ConcatSeq(varIdx), ConcatSeq())
        val conjRangeSets2 = conjRangeSets + (varName -> (conjRangeSets.getOrElse(varName, TermNodeRangeSet.empty) | TermNodeRangeSet(SortedMap(range -> pair))))
        val conjDepthRangeSet2 = conjDepthRangeSet | TermNodeRangeSet(SortedMap(range -> TermNodeRangeValue.empty))
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
            val pair = TermNodeRangeValue(ConcatSeq.fromIterable(idxs), ConcatSeq())
            name -> (disjRangeSets2.getOrElse(name, TermNodeRangeSet.empty) | optRange.map { r => TermNodeRangeSet(SortedMap(r -> pair)) }.getOrElse(TermNodeRangeSet.empty))
        }
        val disjDepthRangeSet2 = disjDepthRangeSet | optRange.map { r => TermNodeRangeSet(SortedMap(r -> TermNodeRangeValue.empty)) }.getOrElse(TermNodeRangeSet.empty)
        (tuple3.copy(_2 = disjRangeSets3, _4 = disjDepthRangeSet2 :: tuple3._4), Map(), optRange)
      case TermLeaf(varName, varIdx) =>
        val range = TermNodeRange(varIdx, varIdx)
        val pair = TermNodeRangeValue(ConcatSeq(varIdx), ConcatSeq())
        val disjRangeSets2 = disjRangeSets + (varName -> (disjRangeSets.getOrElse(varName, TermNodeRangeSet.empty) | TermNodeRangeSet(SortedMap(range -> pair))))
        val disjDepthRangeSet2 = disjDepthRangeSet | TermNodeRangeSet(SortedMap(range -> TermNodeRangeValue.empty))
        (tuple.copy(_2 = disjRangeSets2, _4 = disjDepthRangeSet2 :: nextDisjDepthRangeSets), Map(varName -> Set(varIdx)), some(TermNodeRange(varIdx, varIdx)))
    }
  }
  
  private def rangeSetsFromTermNode(node: TermNode)(tuple: (Map[String, TermNodeRangeSet], Map[String, TermNodeRangeSet], List[TermNodeRangeSet], List[TermNodeRangeSet])) = {
    val (tuple, varIdxs, optRange) = rangeSetsFromConjunctionNode(node)((Map(), Map(), Nil, Nil))
    val (conjRangeSets, disjRangeSets, conjDepthRangeSets, disjDepthRangeSets) = tuple
    val disjRangeSets2 = tuple._2 ++ varIdxs.map { 
      case (name, idxs) => 
        val pair = TermNodeRangeValue(ConcatSeq.fromIterable(idxs), ConcatSeq())
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
            (depthRangeSets2.headOption.map(rs.withValuesFromVarIndex(varIdx).superset).getOrElse(rs.withValuesFromVarIndex(varIdx)))
        }.getOrElse(TermNodeRangeSet.empty)
    }
  }
  
  private def generateCountGraphForConjunction(otherVarIdxs: Map[Int, Set[Int]], node: TermNode, isSuperterm: Boolean)(tuple: (CounterGraph[CounterGraphLocation], Set[(CounterGraphLocation, CounterGraphLocation)], Map[Int, String])): Option[(CounterGraph[CounterGraphLocation], Set[(CounterGraphLocation, CounterGraphLocation)], Map[Int, String])] =
	node match {
	  case TermBranch(childs)        =>
	    (for(fvi <- node.firstIndex; lvi <- node.lastIndex) yield (fvi, lvi)) match {
	      case Some((firstVarIdx, lastVarIdx)) =>
	        val uLoc = CounterGraphLocation(TermNodeRange(firstVarIdx, lastVarIdx), isSuperterm)
	        childs.foldLeft(some(tuple)) {
	          case (Some(newTuple), child) =>
	            generateCountGraphForDisjunction(otherVarIdxs, child, isSuperterm)(newTuple) match {
	              case Some((newG2, newEs2, newVns2)) =>
    	            for(fvi2 <- child.firstIndex; lvi2 <- child.lastIndex) yield {
                      val vLoc = CounterGraphLocation(TermNodeRange(fvi2, lvi2), isSuperterm)
                      (newG2.withTwoEdges(vLoc, uLoc), newEs2, newVns2)
                    }
	              case None                           =>
	                none
	            }
	          case (None, _)               =>
	            none
	        }.map { case (g2, es2, vns2) => (if(childs.size > 1) g2.withCount(uLoc, 1) else g2, es2, vns2) }
	      case None                            =>
	        none
	    }
	  case TermLeaf(varName, varIdx) =>
	    generateCountGraphForDisjunction(otherVarIdxs, node, isSuperterm)(tuple)
	}

  private def generateCountGraphForDisjunction(otherVarIdxs: Map[Int, Set[Int]], node: TermNode, isSuperterm: Boolean)(tuple: (CounterGraph[CounterGraphLocation], Set[(CounterGraphLocation, CounterGraphLocation)], Map[Int, String])): Option[(CounterGraph[CounterGraphLocation], Set[(CounterGraphLocation, CounterGraphLocation)], Map[Int, String])] =
	node match {
	  case TermBranch(childs)        =>
	    (for(fvi <- node.firstIndex; lvi <- node.lastIndex) yield (fvi, lvi)) match {
	      case Some((firstVarIdx, lastVarIdx)) =>
	        val uLoc = CounterGraphLocation(TermNodeRange(firstVarIdx, lastVarIdx), isSuperterm)
	        childs.foldLeft(some(tuple)) {
	          case (Some(newTuple), child) =>
	            generateCountGraphForConjunction(otherVarIdxs, child, isSuperterm)(newTuple) match {
	              case Some((newG2, newEs2, newVns2)) =>
    	            for(fvi2 <- child.firstIndex; lvi2 <- child.lastIndex) yield {
                      val vLoc = CounterGraphLocation(TermNodeRange(fvi2, lvi2), isSuperterm)
                      (newG2.withTwoEdges(vLoc, uLoc), newEs2, newVns2)
                    }
	              case None                           =>
	                none
	            }
	          case (None, _)               =>
	            none
	        }.map { case (g2, es2, vns2) => (if(childs.size > 1) g2.withCount(uLoc, childs.size) else g2, es2, vns2) }
	      case None                            =>
	        none
	    }
	  case TermLeaf(varName, varIdx) =>
	    val (g, es, vns) = tuple
	    val vLoc = CounterGraphLocation(TermNodeRange(varIdx, varIdx), isSuperterm)
	    if(otherVarIdxs.contains(varIdx)) {
	      val es2 =  otherVarIdxs.get(varIdx).toSet.flatMap { 
	        _.map { i => vLoc -> CounterGraphLocation(TermNodeRange(i, i), !isSuperterm) }
	      }
	      some((g.withCount(vLoc, 1), es ++ es2, vns + (varIdx -> varName)))
	    } else
	      some((g.withCount(vLoc, 0), es, vns))
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
  
  private def checkSuperconjunctionNodeFromTerms(term1: MatchingTerm, term2: MatchingTerm, isFirstTry: Boolean) =
    (term1.conjNode, term2.conjNode) match {
      case (TermBranch(childs1), TermBranch(childs2)) if childs1.size > 1 && childs2.size === 1 && !isFirstTry =>
        val conjDepthRangeSets = TermNodeRangeSet.full :: term2.conjDepthRangeSets
        checkSuperconjunctionNode(TermBranch(Vector(TermBranch(Vector(term1.conjNode)))), term2.conjRangeSets, conjDepthRangeSets)
      case (TermBranch(childs1), TermBranch(childs2)) if childs1.size === 1 && childs2.size > 1 && isFirstTry =>
        val conjDepthRangeSet = term2.conjDepthRangeSets.headOption.getOrElse(TermNodeRangeSet.full)
        val conjDepthRangeSets = TermNodeRangeSet.full :: conjDepthRangeSet :: term2.conjDepthRangeSets
        checkSuperconjunctionNode(term1.conjNode, term2.conjRangeSets, conjDepthRangeSets)
      case _ =>
        val conjDepthRangeSets = TermNodeRangeSet.full :: term2.conjDepthRangeSets
        checkSuperconjunctionNode(term1.conjNode, term2.conjRangeSets, conjDepthRangeSets)
    }
  
  private def checkSuperdisjunctionNodeFromTerms(term1: MatchingTerm, term2: MatchingTerm, isFirstTry: Boolean) =
    (term1.conjNode, term2.conjNode) match {
      case (TermBranch(childs1), TermBranch(childs2)) if childs1.size > 1 && childs2.size === 1 && isFirstTry =>
        val disjDepthRangeSets = TermNodeRangeSet.full :: TermNodeRangeSet.full :: term2.disjDepthRangeSets
        checkSuperdisjunctionNode(TermBranch(Vector(TermBranch(Vector(term1.conjNode)))), term2.disjRangeSets, disjDepthRangeSets)
      case (TermBranch(childs1), TermBranch(childs2)) if childs1.size === 1 && childs2.size > 1 && !isFirstTry =>
        val disjDepthRangeSet = term2.disjDepthRangeSets.headOption.getOrElse(TermNodeRangeSet.full)
        val disjDepthRangeSets = TermNodeRangeSet.full :: TermNodeRangeSet.full :: disjDepthRangeSet :: term2.disjDepthRangeSets
        checkSuperdisjunctionNode(term1.conjNode, term2.disjRangeSets, disjDepthRangeSets)
      case _ =>
        val disjDepthRangeSets = TermNodeRangeSet.full :: TermNodeRangeSet.full :: term2.disjDepthRangeSets
        checkSuperdisjunctionNode(term1.conjNode, term2.disjRangeSets, disjDepthRangeSets)
    }
  
  private def partiallyMatchesSupertermWithTerm(term1: MatchingTerm, term2: MatchingTerm, isFirstTry: Boolean) = {
    val conjRangeSet = checkSuperconjunctionNodeFromTerms(term1, term2, isFirstTry)
    val disjRangeSet = checkSuperdisjunctionNodeFromTerms(term2, term1, isFirstTry)
    if(!conjRangeSet.isEmpty && !disjRangeSet.isEmpty) {
      val conjOtherVarIdxs = conjRangeSet.value.varIdxPairs.toSet.foldLeft(Map[Int, Set[Int]]()) {
        case (is, (i, j)) => is |+| Map(i -> Set(j))
      }
      val disjOtherVarIdxs = disjRangeSet.value.varIdxPairs.toSet.foldLeft(Map[Int, Set[Int]]()) {
        case (is, (i, j)) => is |+| Map(i -> Set(j))
      }
      for {
        (graph, conjEdges, conjVarNames) <- generateCountGraphForConjunction(conjOtherVarIdxs, term1.conjNode, true)((CounterGraph.empty, Set(), Map()))
        (graph2, disjEdges, disjVarNames) <- generateCountGraphForDisjunction(disjOtherVarIdxs, term2.conjNode, false)((graph, Set(), Map()))
        varNames2 <- {
          val graph3 = counterGraphWithTwoVarEdgeSets(graph2, conjEdges, disjEdges)
          graph3.decreaseCounters.flatMap {
            graph4 =>
              for {
                conjLastVarIdx <- term1.conjNode.lastIndex
                disjLastVarIdx <- term2.conjNode.lastIndex
                varNames <- {
                  val vLoc = CounterGraphLocation(TermNodeRange(0, conjLastVarIdx), true)
                  val uLoc = CounterGraphLocation(TermNodeRange(0, disjLastVarIdx), false)
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
              } yield varNames
          }
        }
      } yield varNames2
    } else
      none
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
