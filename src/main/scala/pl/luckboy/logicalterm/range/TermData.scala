package pl.luckboy.logicalterm.range
import pl.luckboy.logicalterm._

case class TermData(
    conjVarIdxs: Map[Int, Int] /* narrowest */,
    disjVarIdxs: Map[Int, Int] /* narrowest */,
    varArgs: Map[String, Vector[Term]],
    conjNodeIdx: Int,
    disjNodeIdx: Int,
    nextNodeIdx: Int,
    nextVarIdx: Int)
    
object LogicalOp extends Enumeration
{
  val Conjunction, Disjunction = Value
  
}