package pl.luckboy.logicalterm.range
import scala.collection.immutable.SortedSet

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
  def & (rangeSet: TermNodeRangeSet): TermNodeRangeSet =
    throw new UnsupportedOperationException
  
  def | (rangeSet: TermNodeRangeSet): TermNodeRangeSet =
    throw new UnsupportedOperationException
    
  def isEmpty: Boolean =
    throw new UnsupportedOperationException
    
  def superset(rangeSet: TermNodeRangeSet): TermNodeRangeSet =
    throw new UnsupportedOperationException
    
  def subset(rangeSet: TermNodeRangeSet): TermNodeRangeSet =
    throw new UnsupportedOperationException
  
  def supersetAndSubset(sepRageSet: TermNodeRangeSet): (TermNodeRangeSet, TermNodeRangeSet) =
    throw new UnsupportedOperationException
}

object TermNodeRangeSet
{
  def empty: TermNodeRangeSet =
    throw new UnsupportedOperationException
  
  def full: TermNodeRangeSet = 
    throw new UnsupportedOperationException
}

case class TermNodeRange(minIdx: Int, maxIdx: Int)