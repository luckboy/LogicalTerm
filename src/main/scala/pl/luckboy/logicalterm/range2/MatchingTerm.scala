package pl.luckboy.logicalterm.range2
import scala.collection.immutable.SortedMap
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.logicalterm.ConcatSeq

case class MatchingTerm(
    conjNode: TermNode,
    conjRangeSets: Map[String, TermNodeRangeSet] /* narrowest */,
    disjRangeSets: Map[String, TermNodeRangeSet] /* narrowest */,
    conjDepthRangeSets: List[TermNodeRangeSet],
    disjDepthRangeSets: List[TermNodeRangeSet],
    varArgs: Map[String, Vector[MatchingTerm]])
{
  def toSimpleString = conjNode.toConjunctionStringForVarArgs(varArgs)
  
  def toArgString =
    conjNode match {
      case TermLeaf(varName, _) =>
        if(varArgs.get(varName).map { _.isEmpty }.getOrElse(false)) toSimpleString else "(" + this.toSimpleString + ")"
      case _                    =>
        "(" + this.toString + ")"
    }
    
  override def toString =
    toSimpleString + "\n" + 
    "// conjRangeSets=Map(" + conjRangeSets.map { case (n, rs) => n + "->" + rs }.mkString(",") + ")\n" +
    "// disjRangeSets=Map(" + disjRangeSets.map { case (n, rs) => n + "->" + rs }.mkString(",") + ")\n" +
    "// conjDepthRangeSets=List(" + conjDepthRangeSets.mkString(",") + ")\n" +
    "// disjDepthRangeSets=List(" + disjDepthRangeSets.mkString(",") + ")"
}

sealed trait TermNode
{
  def withChild(child: TermNode) =
    this match {
      case TermBranch(childs) => TermBranch(childs :+ child)
      case TermLeaf(_, _)     => TermBranch(Vector(this, child))
    }
  
  def withIndexesFromIndex(idx: Int): (Int, TermNode) =
    this match {
      case TermBranch(childs) =>
        childs.foldLeft((idx, Vector[TermNode]())) {
          case ((newIdx, newChilds), child) => 
            child.withIndexesFromIndex(newIdx).mapElements(identity, newChilds :+ _)
        }.mapElements(identity, TermBranch(_))
      case TermLeaf(varName, _) =>
        (idx + 1, TermLeaf(varName, idx))
    }
  
  def withIndexes = withIndexesFromIndex(0)._2
  
  def toConjunctionStringForVarArgs(varArgs: Map[String, Vector[MatchingTerm]]): String =
    this match {
      case TermBranch(childs) =>
        childs.map { _.toDisjunctionStringForVarArgs(varArgs) }.mkString(" & ")
      case TermLeaf(varName, varIdx) =>
        varName + "/*" + varIdx + "*/" + varArgs.get(varName).map { _.map { " " + _.toArgString }.mkString("") }.getOrElse("/* not found arguments */")
    }

  def toDisjunctionStringForVarArgs(varArgs: Map[String, Vector[MatchingTerm]]): String =
    this match {
      case TermBranch(childs) =>
        "(" + childs.map { _.toConjunctionStringForVarArgs(varArgs) }.mkString(" | ") + ")"
      case TermLeaf(varName, varIdx) =>
        varName + "/*" + varIdx + "*/" + varArgs.get(varName).map { _.map { " " + _.toArgString }.mkString("") }.getOrElse("/* not found arguments */")
    }
}

case class TermBranch(childs: Vector[TermNode]) extends TermNode

case class TermLeaf(varName: String, varIdx: Int) extends TermNode

case class TermNodeRangeSet(ranges: SortedMap[TermNodeRange, VarIndexSeqPair])
{ 
  private def intersect(rangeSet: TermNodeRangeSet) = {
    val newRanges3 = ranges.foldLeft(SortedMap[TermNodeRange, VarIndexSeqPair]()) {
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
    val newRanges2 = ranges.foldLeft(SortedMap[TermNodeRange, VarIndexSeqPair]()) {
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
    val newRanges2 = ranges.foldLeft(SortedMap[TermNodeRange, VarIndexSeqPair]()) {
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
    
  def swapPairsWithMyVarIndex(idx: Int) =
    TermNodeRangeSet(ranges.mapValues { case VarIndexSeqPair(oldMyVarIdxs, _) => VarIndexSeqPair(ConcatSeq(idx), oldMyVarIdxs) })
    
  def varIndexSeqPair = 
    ranges.values.foldLeft(VarIndexSeqPair.empty) { (v, v2) => v2 ++ v }
  
  override def toString = "{" + ranges.map { case (r, p) => r + "->" + p }.mkString(",") + "}"
}

object TermNodeRangeSet
{
  val empty = TermNodeRangeSet(SortedMap())
  
  val full = TermNodeRangeSet(SortedMap(TermNodeRange.full -> VarIndexSeqPair.empty))
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

case class VarIndexSeqPair(myVarIdxs: ConcatSeq[Int], otherVarIdxs: ConcatSeq[Int])
{
  def ++ (pair: VarIndexSeqPair) = VarIndexSeqPair(myVarIdxs ++ pair.myVarIdxs, otherVarIdxs ++ pair.otherVarIdxs)

  override def toString = "({" + myVarIdxs.toSet.mkString(",") + "},{" + otherVarIdxs.toSet.mkString(",") + "})"
}

object VarIndexSeqPair
{
  val empty = VarIndexSeqPair(ConcatSeq.empty, ConcatSeq.empty)
}