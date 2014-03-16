package pl.luckboy.logicalterm.range
import scala.collection.immutable.SortedSet

case class MatchingTerm(
    conjNode: TermNode,
    conjRangeSets: Map[String, TermNodeRangeSet] /* narrowest */,
    disjRangeSets: Map[String, TermNodeRangeSet] /* narrowest */,
    varArgs: Map[String, Vector[MatchingTerm]])

sealed trait TermNode

case class TermBranch(childs: Vector[TermNode]) extends TermNode

case class TermLeaf(varName: String) extends TermNode

case class TermNodeRangeSet(ranges: SortedSet[TermNodeRange])
{
  def & (rangeSet: TermNodeRangeSet): TermNodeRangeSet =
    throw new UnsupportedOperationException
  
  def | (rangeSet: TermNodeRangeSet): TermNodeRangeSet =
    throw new UnsupportedOperationException
  
  def isEmpty: Boolean =
    throw new UnsupportedOperationException
}

object TermNodeRangeSet
{
  def empty: TermNodeRangeSet = 
    throw new UnsupportedOperationException
}

case class TermNodeRange(minIdx: Int, maxIdx: Int)