package pl.luckboy.logicalterm.undistributed
import scala.collection.immutable.IntMap
import scala.collection.immutable.SortedMap
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.logicalterm.Term

case class MatchingTerm(
    root: TermNode,
    varIdxs: Map[String, List[Int]])


case class TermNode(
    childs: SortedMap[TermNodeRange, TermNode],
    firstVarName: String)

case class TermNodeRange(minIdx: Int, maxIdx: Int)