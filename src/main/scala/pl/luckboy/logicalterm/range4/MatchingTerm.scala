/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.logicalterm.range4
import scala.collection.immutable.SortedMap
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.logicalterm.ConcatSeq

case class MatchingTerm(
    conjNode: TermNode,
    varArgs: Map[String, Vector[MatchingTerm]])
{
  lazy val info = MatchingTermInfo.fromTermNodeAndVarArgs(conjNode, varArgs)
  
  def toSimpleString = conjNode.toConjunctionStringForVarArgs(varArgs)
  
  def toArgString =
    conjNode match {
      case TermLeaf(varName) =>
        if(varArgs.get(varName).map { _.isEmpty }.getOrElse(false)) toSimpleString else "(" + this.toSimpleString + ")"
      case _                 =>
        "(" + this.toString + ")"
    }
    
  override def toString =
    toSimpleString + "\n" + 
    "// info.conjRangeSets=Map(" + info.conjRangeSets.map { case (n, rs) => n + "->" + rs }.mkString(",") + ")\n" +
    "// info.disjRangeSets=Map(" + info.disjRangeSets.map { case (n, rs) => n + "->" + rs }.mkString(",") + ")\n" +
    "// info.conjDepthRangeSets=List(" + info.conjDepthRangeSets.mkString(",") + ")\n" +
    "// info.disjDepthRangeSets=List(" + info.disjDepthRangeSets.mkString(",") + ")"
}

case class MatchingTermInfo(
    conjRangeSets: Map[String, TermNodeRangeSet] /* narrowest */,
    disjRangeSets: Map[String, TermNodeRangeSet] /* narrowest */,
    conjDepthRangeSets: List[TermNodeRangeSet],
    disjDepthRangeSets: List[TermNodeRangeSet])

object MatchingTermInfo
{
  private def rangeSetsFromConjunctionNode(node: TermNode)(varIdx: Int)(tuple: (Map[String, TermNodeRangeSet], Map[String, TermNodeRangeSet], List[TermNodeRangeSet], List[TermNodeRangeSet])): ((Map[String, TermNodeRangeSet], Map[String, TermNodeRangeSet], List[TermNodeRangeSet], List[TermNodeRangeSet]), Map[String, Set[Int]], Option[TermNodeRange]) = {
    val (conjRangeSets, disjRangeSets, conjDepthRangeSets, disjDepthRangeSets) = tuple
    val (conjDepthRangeSet, nextConjDepthRangeSets) = conjDepthRangeSets.headOption.map {
     (_, conjDepthRangeSets.tail)
    }.getOrElse(TermNodeRangeSet.empty, Nil)
    node match {
      case TermBranch(childs, _) =>
        val tuple2 = tuple.copy(_3 = nextConjDepthRangeSets)
        val (tuple3, _, varIdxs, optRange) = childs.foldLeft((tuple2, varIdx, Map[String, Set[Int]](), none[TermNodeRange])) {
          case ((newTuple, newVarIdx, newVarIdxs, optNewRange), child) =>
            val (newTuple2, newVarIdxs2, optNewRange2) = rangeSetsFromDisjunctionNode(child)(newVarIdx)(newTuple)
            (newTuple2, newVarIdx + child.varCount, newVarIdxs |+| newVarIdxs2, (optNewRange |@| optNewRange2) { _ | _ }.orElse(optNewRange2))
        }
        val conjRangeSets2 = tuple3._1
        val conjRangeSets3 = conjRangeSets2 ++ varIdxs.map { 
          case (name, idxs) => 
            val pair = TermNodeRangeValue(ConcatSeq.fromIterable(idxs), ConcatSeq())
            name -> (conjRangeSets2.getOrElse(name, TermNodeRangeSet.empty) | optRange.map { r => TermNodeRangeSet(SortedMap(r -> pair)) }.getOrElse(TermNodeRangeSet.empty))
        }
        val conjDepthRangeSet2 = conjDepthRangeSet | optRange.map { r => TermNodeRangeSet(SortedMap(r -> TermNodeRangeValue.empty)) }.getOrElse(TermNodeRangeSet.empty)
        (tuple3.copy(_1 = conjRangeSets3, _3 = conjDepthRangeSet2 :: tuple3._3), Map(), optRange)
      case TermLeaf(varName) =>
        val range = TermNodeRange(varIdx, varIdx)
        val pair = TermNodeRangeValue(ConcatSeq(varIdx), ConcatSeq())
        val conjRangeSets2 = conjRangeSets + (varName -> (conjRangeSets.getOrElse(varName, TermNodeRangeSet.empty) | TermNodeRangeSet(SortedMap(range -> pair))))
        val conjDepthRangeSet2 = conjDepthRangeSet | TermNodeRangeSet(SortedMap(range -> TermNodeRangeValue.empty))
        (tuple.copy(_1 = conjRangeSets2, _3 = conjDepthRangeSet2 :: nextConjDepthRangeSets), Map(varName -> Set(varIdx)), some(TermNodeRange(varIdx, varIdx)))
    }
  }

  private def rangeSetsFromDisjunctionNode(node: TermNode)(varIdx: Int)(tuple: (Map[String, TermNodeRangeSet], Map[String, TermNodeRangeSet], List[TermNodeRangeSet], List[TermNodeRangeSet])): ((Map[String, TermNodeRangeSet], Map[String, TermNodeRangeSet], List[TermNodeRangeSet], List[TermNodeRangeSet]), Map[String, Set[Int]], Option[TermNodeRange]) = {
    val (conjRangeSets, disjRangeSets, conjDepthRangeSets, disjDepthRangeSets) = tuple
    val (disjDepthRangeSet, nextDisjDepthRangeSets) = disjDepthRangeSets.headOption.map {
      (_, disjDepthRangeSets.tail)
    }.getOrElse(TermNodeRangeSet.empty, Nil)
    node match {
      case TermBranch(childs, _) =>
        val tuple2 = tuple.copy(_4 = nextDisjDepthRangeSets)
        val (tuple3, _, varIdxs, optRange) = childs.foldLeft((tuple2, varIdx, Map[String, Set[Int]](), none[TermNodeRange])) {
          case ((newTuple, newVarIdx, newVarIdxs, optNewRange), child) =>
            val (newTuple2, newVarIdxs2, optNewRange2) = rangeSetsFromConjunctionNode(child)(newVarIdx)(newTuple)
            (newTuple2, newVarIdx + child.varCount, newVarIdxs |+| newVarIdxs2, (optNewRange |@| optNewRange2) { _ | _ }.orElse(optNewRange2))
        }
        val disjRangeSets2 = tuple3._2
        val disjRangeSets3 = disjRangeSets2 ++ varIdxs.map { 
          case (name, idxs) => 
            val pair = TermNodeRangeValue(ConcatSeq.fromIterable(idxs), ConcatSeq())
            name -> (disjRangeSets2.getOrElse(name, TermNodeRangeSet.empty) | optRange.map { r => TermNodeRangeSet(SortedMap(r -> pair)) }.getOrElse(TermNodeRangeSet.empty))
        }
        val disjDepthRangeSet2 = disjDepthRangeSet | optRange.map { r => TermNodeRangeSet(SortedMap(r -> TermNodeRangeValue.empty)) }.getOrElse(TermNodeRangeSet.empty)
        (tuple3.copy(_2 = disjRangeSets3, _4 = disjDepthRangeSet2 :: tuple3._4), Map(), optRange)
      case TermLeaf(varName) =>
        val range = TermNodeRange(varIdx, varIdx)
        val pair = TermNodeRangeValue(ConcatSeq(varIdx), ConcatSeq())
        val disjRangeSets2 = disjRangeSets + (varName -> (disjRangeSets.getOrElse(varName, TermNodeRangeSet.empty) | TermNodeRangeSet(SortedMap(range -> pair))))
        val disjDepthRangeSet2 = disjDepthRangeSet | TermNodeRangeSet(SortedMap(range -> TermNodeRangeValue.empty))
        (tuple.copy(_2 = disjRangeSets2, _4 = disjDepthRangeSet2 :: nextDisjDepthRangeSets), Map(varName -> Set(varIdx)), some(TermNodeRange(varIdx, varIdx)))
    }
  }
  
  private def rangeSetsFromTermNode(node: TermNode)(tuple: (Map[String, TermNodeRangeSet], Map[String, TermNodeRangeSet], List[TermNodeRangeSet], List[TermNodeRangeSet])) = {
    val (tuple, varIdxs, optRange) = rangeSetsFromConjunctionNode(node)(0)((Map(), Map(), Nil, Nil))
    val (conjRangeSets, disjRangeSets, conjDepthRangeSets, disjDepthRangeSets) = tuple
    val disjRangeSets2 = tuple._2 ++ varIdxs.map { 
      case (name, idxs) => 
        val pair = TermNodeRangeValue(ConcatSeq.fromIterable(idxs), ConcatSeq())
        name -> (disjRangeSets.getOrElse(name, TermNodeRangeSet.empty) | optRange.map { r => TermNodeRangeSet(SortedMap(r -> pair)) }.getOrElse(TermNodeRangeSet.empty))
    }
    tuple.copy(_2 = disjRangeSets2)
  }
  
  def fromTermNodeAndVarArgs(node: TermNode, varArgs: Map[String, Vector[MatchingTerm]]) = {
    val (conjRangeSet, disjRangeSet, conjDepthRangeSets, disjDepthRangeSets) = rangeSetsFromTermNode(node)((Map(), Map(), Nil, Nil))
    MatchingTermInfo(conjRangeSet, disjRangeSet, conjDepthRangeSets, disjDepthRangeSets)
  }
}

sealed trait TermNode
{
  def varCount: Int
  
  def withChild(child: TermNode) =
    this &| TermBranch(Vector(child), child.varCount).normalizedTermNode
  
  def &| (node: TermNode) =
    (this, node) match {
      case (TermBranch(childs1, varCount1), TermBranch(childs2, varCount2)) =>
        TermBranch(childs1 ++ childs2, varCount1 + varCount2 )
      case (TermBranch(childs1, varCount1), TermLeaf(_))                    =>
        TermBranch(childs1 :+ node, varCount1 + 1)
      case (TermLeaf(_), TermBranch(childs2, varCount2))                    =>
        TermBranch(this +: childs2, varCount2 + 1)
      case (TermLeaf(_), TermLeaf(_))                                       =>
        TermBranch(Vector(this, node), 2)
    }
  
  private def withIndexesFromIndex(idx: Int): (Int, TermNode) =
    this match {
      case TermBranch(childs, _) =>
        childs.foldLeft((idx, Vector[TermNode]())) {
          case ((newIdx, newChilds), child) => 
            child.withIndexesFromIndex(newIdx).mapElements(identity, newChilds :+ _)
        }.mapElements(identity, TermBranch(_, varCount))
      case TermLeaf(varName)     =>
        (idx + 1, TermLeaf(varName))
    }
  
  def withIndexes = withIndexesFromIndex(0)._2
  
  def normalizedTermNode: TermNode =
    this match {
      case TermBranch(Vector(child), _) =>
        child match {
          case TermBranch(Vector(child2), _)  => child2.normalizedTermNode
          case TermBranch(_, childVarCount)   => TermBranch(Vector(child), childVarCount)
          case TermLeaf(_)                    => child
        }
      case _                            =>
        this
    }
  
  def toConjunctionStringForVarArgs(varArgs: Map[String, Vector[MatchingTerm]]): String =
    this match {
      case TermBranch(childs, _) =>
        childs.map { _.toDisjunctionStringForVarArgs(varArgs) }.mkString(" & ") + " /*" + varCount + "*/"
      case TermLeaf(varName) =>
        varName + varArgs.get(varName).map { _.map { " " + _.toArgString }.mkString("") }.getOrElse("/* not found arguments */")
    }

  def toDisjunctionStringForVarArgs(varArgs: Map[String, Vector[MatchingTerm]]): String =
    this match {
      case TermBranch(childs, _) =>
        "(" + childs.map { _.toConjunctionStringForVarArgs(varArgs) }.mkString(" | ") + " /*" + varCount + "*/)"
      case TermLeaf(varName) =>
        varName  + varArgs.get(varName).map { _.map { " " + _.toArgString }.mkString("") }.getOrElse("/* not found arguments */")
    }
}

case class TermBranch(childs: Vector[TermNode], varCount: Int) extends TermNode

case class TermLeaf(varName: String) extends TermNode
{
  override def varCount = 1
}

case class TermNodeRangeSet(ranges: SortedMap[TermNodeRange, TermNodeRangeValue])
{ 
  private def intersect(rangeSet: TermNodeRangeSet) = {
    val newRanges3 = ranges.foldLeft(SortedMap[TermNodeRange, TermNodeRangeValue]()) {
      case (newRanges, (range, pair)) =>
        val from = TermNodeRange(range.minIdx, range.minIdx)
        val to = TermNodeRange(range.maxIdx, range.maxIdx)
        rangeSet.ranges.from(from).to(to).foldLeft(newRanges) {
          case (newRanges2, (range2, pair2)) =>
            if(range.minIdx >= range2.minIdx && range.maxIdx <= range2.maxIdx)
              newRanges2 + (range -> (pair2 ++ pair))
            else if(range.minIdx <= range2.minIdx && range.maxIdx >= range2.maxIdx)
              newRanges2 + (range2 -> (pair2 ++ pair))
            else
              newRanges2 // this case is impossible
        }
    }
    TermNodeRangeSet(newRanges3)
  }
  
  def & (rangeSet: TermNodeRangeSet) =
    if(ranges.size < rangeSet.ranges.size) intersect(rangeSet) else rangeSet.intersect(this)
  
  private def union(rangeSet: TermNodeRangeSet) = {
    val newRanges2 = ranges.foldLeft(SortedMap[TermNodeRange, TermNodeRangeValue]()) {
      case (newRanges, (range, pair)) =>
        val from = TermNodeRange(range.minIdx, range.minIdx)
        val to = TermNodeRange(range.maxIdx, range.maxIdx)
        val ranges2 = rangeSet.ranges.from(from).to(to)
        ranges2.headOption.map {
          case (range2, pair2) =>
            if(range.minIdx <= range2.minIdx && range.maxIdx >= range2.maxIdx)
              newRanges + (range -> ranges2.values.foldLeft(pair) { (p, p2) => p2 ++ p })
            else if(range.minIdx >= range2.minIdx && range.maxIdx <= range2.maxIdx) {
              newRanges + (range2 -> (newRanges.get(range).map(pair ++ pair2 ++).getOrElse(pair ++ pair2)))
            } else
              newRanges // this case is impossible
        }.getOrElse(newRanges)
    }
    val newRanges4 = ranges.foldLeft(newRanges2) {
      case (newRanges3, pair @ (range, value)) => if(newRanges2.contains(range)) newRanges3 else newRanges3 + pair
    }
    val newRanges6 = rangeSet.ranges.foldLeft(newRanges4) {
      case (newRanges5, pair @ (range, value)) => if(newRanges2.contains(range)) newRanges5 else newRanges5 + pair
    }
    TermNodeRangeSet(newRanges6)
  }
  
  def | (rangeSet: TermNodeRangeSet) =
    if(ranges.size < rangeSet.ranges.size) union(rangeSet) else rangeSet.union(this)
    
  def isEmpty = ranges.isEmpty
    
  def superset(sepRangeSet: TermNodeRangeSet) = {
    val newRanges2 = ranges.foldLeft(SortedMap[TermNodeRange, TermNodeRangeValue]()) {
      case (newRanges, (range, pair)) =>
        val from = TermNodeRange(range.minIdx, range.minIdx)
        val to = TermNodeRange(range.maxIdx, range.maxIdx)
        val sepRanges = sepRangeSet.ranges.from(from).to(to)
        sepRanges.headOption.map {
          case (sepRange, sepPair) =>
            if(range.minIdx <= sepRange.minIdx && range.maxIdx >= sepRange.maxIdx)
              newRanges + (range -> sepRanges.values.foldLeft(pair) { (p, p2) => p2 ++ p })
            else if(range.minIdx >= sepRange.minIdx && range.maxIdx <= sepRange.maxIdx)
              newRanges + (sepRange -> (newRanges.get(range).map(pair ++ sepPair ++).getOrElse(pair ++ sepPair)))
            else
              newRanges // this case is impossible
        }.getOrElse(newRanges)
    }
    val newRanges4 = ranges.foldLeft(newRanges2) {
      case (newRanges3, pair @ (range, value)) => if(newRanges2.contains(range)) newRanges3 else newRanges3 + pair
    }
    TermNodeRangeSet(newRanges4)
  }
    
  def withValuesFromVarIndex(idx: Int) =
    TermNodeRangeSet(ranges.mapValues { v => v.copy(varIdxPairs = v.myVarIdxs.map { idx -> _ }) })
    
  def value = ranges.values.foldLeft(TermNodeRangeValue.empty) { (v, v2) => v2 ++ v }
  
  override def toString = "{" + ranges.map { case (r, p) => r + "->" + p }.mkString(",") + "}"
}

object TermNodeRangeSet
{
  val empty = TermNodeRangeSet(SortedMap())
  
  val full = TermNodeRangeSet(SortedMap(TermNodeRange.full -> TermNodeRangeValue.empty))
}

case class TermNodeRange(minIdx: Int, maxIdx: Int)
{
  def | (range: TermNodeRange) = TermNodeRange(minIdx.min(range.minIdx), maxIdx.max(range.maxIdx))
  
  override def toString = "[" + minIdx + "," + (if(maxIdx === Integer.MAX_VALUE) "max" else maxIdx) + "]"
}

object TermNodeRange
{
  val full = TermNodeRange(0, Integer.MAX_VALUE)
}

case class CounterGraphLocation(range: TermNodeRange, isSuperterm: Boolean)
{
  override def toString = "(" + range + "," + isSuperterm + ")"
  
  override lazy val hashCode = range.hashCode ^ isSuperterm.hashCode
}

case class TermNodeRangeValue(
    otherVarIdxs: ConcatSeq[Int],
    varIdxPairs: ConcatSeq[(Int, Int)])
{
  def myVarIdxs = otherVarIdxs
  
  def ++ (pair: TermNodeRangeValue) = TermNodeRangeValue(otherVarIdxs ++ pair.otherVarIdxs, varIdxPairs ++ pair.varIdxPairs)

  override def toString = "{" + otherVarIdxs.toSet.mkString(",") + ",{" + varIdxPairs.toSet.mkString(",") + "})"
}

object TermNodeRangeValue
{
  val empty = TermNodeRangeValue(ConcatSeq.empty, ConcatSeq.empty)
}
