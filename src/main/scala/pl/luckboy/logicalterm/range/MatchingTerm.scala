package pl.luckboy.logicalterm.range
import scala.collection.immutable.SortedSet
import scalaz._
import scalaz.Scalaz._

case class MatchingTerm(
    conjNode: TermNode,
    conjRangeSets: Map[String, TermNodeRangeSet] /* narrowest */,
    disjRangeSets: Map[String, TermNodeRangeSet] /* narrowest */,
    conjDepthRangeSets: List[TermNodeRangeSet],
    disjDepthRangeSets: List[TermNodeRangeSet],
    varArgs: Map[String, Vector[MatchingTerm]])

sealed trait TermNode

case class TermBranch(childs: Vector[TermNode]) extends TermNode

case class TermLeaf(varName: String) extends TermNode

case class TermNodeRangeSet(ranges: SortedSet[TermNodeRange])
{
  private def intersect(rangeSet: TermNodeRangeSet) = {
    val newRanges = ranges.flatMap {
      range =>
        val from = TermNodeRange(range.minIdx, range.minIdx)
        val to = TermNodeRange(range.maxIdx, range.maxIdx)
        rangeSet.ranges.from(from).to(to).flatMap {
          range2 =>
            if(range.minIdx >= range2.minIdx && range.maxIdx <= range2.maxIdx) SortedSet(range)
            else if(range.minIdx <= range2.minIdx && range.maxIdx >= range2.maxIdx) SortedSet(range2)
            else SortedSet() // this case is impossible
        }
    }
    TermNodeRangeSet(newRanges)
  }
  
  def & (rangeSet: TermNodeRangeSet) =
    if(ranges.size < rangeSet.ranges.size) intersect(rangeSet) else rangeSet.intersect(this)
  
  private def union(rangeSet: TermNodeRangeSet) = {
    val newRanges = ranges.flatMap {
      range =>
        val from = TermNodeRange(range.minIdx, range.minIdx)
        rangeSet.ranges.from(from).headOption.map {
          range2 =>
            if(range.minIdx <= range2.minIdx && range.maxIdx >= range2.maxIdx) SortedSet(range)
            else if(range.minIdx >= range2.minIdx && range.maxIdx <= range2.maxIdx) SortedSet(range2)
            else SortedSet() // this case is impossible
        }.getOrElse(SortedSet(range))
    }
    val newRanges3 = rangeSet.ranges.foldLeft(newRanges) {
      (newRanges2, range) => if(newRanges2.contains(range)) newRanges2 else newRanges2 + range
    }
    TermNodeRangeSet(newRanges3)
  }
  
  def | (rangeSet: TermNodeRangeSet) =
    if(ranges.size < rangeSet.ranges.size) union(rangeSet) else rangeSet.union(this)
    
  def isEmpty = ranges.isEmpty
    
  def superset(sepRangeSet: TermNodeRangeSet) = {
    val newRanges = ranges.flatMap {
      range =>
        val from = TermNodeRange(range.minIdx, range.minIdx)
        sepRangeSet.ranges.from(from).headOption.map {
          sepRange =>
            if(range.minIdx <= sepRange.minIdx && range.maxIdx >= sepRange.maxIdx) SortedSet(range)
            else if(range.minIdx >= sepRange.minIdx && range.maxIdx <= sepRange.maxIdx) SortedSet(sepRange)
            else SortedSet() // this case is impossible
        }.getOrElse(SortedSet(range))
    }
    TermNodeRangeSet(newRanges)
  }    
}

object TermNodeRangeSet
{
  def empty = TermNodeRangeSet(SortedSet())
  
  def full = {
    val fullRange = TermNodeRange(0, Integer.MAX_VALUE)
    TermNodeRangeSet(SortedSet(fullRange))
  }
}

case class TermNodeRange(minIdx: Int, maxIdx: Int)