package pl.luckboy.logicalterm
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._

abstract class Executor
{
  type MatchingTerm

  type Table[T]
  
  def matcher: Matcher[MatchingTerm]
  
  def tabular: Tabular[Table, MatchingTerm]
  
  def emptyTable = tabular.empty[Int]
  
  def matchingTermFromTerm(term: Term) = matcher.matchingTermFromTerm(term)
  
  def executeInstruction(instr: Instruction)(table: Table[Int]) =
    instr match {
      case Match(term1, term2, matching) =>
        (for {
          matchingTerm1 <- matcher.matchingTermFromTerm(term1)
          matchingTerm2 <- matcher.matchingTermFromTerm(term2)
        } yield {
          matcher.matches(matchingTerm1, matchingTerm2, matching).map {
            b => (table, if(b) MatchedTermResult else MismatchedTermResult)
          }
        }).getOrElse(Error("unmatchable term", NoPosition).failure)
      case Find(term) =>
        matcher.matchingTermFromTerm(term).map {
          matchingTerm =>
            tabular.find(table, matchingTerm).map {
              res => (table, res.map { FoundValueResult(_) }.valueOr { NotFoundValueResult(_) })
            }
        }.getOrElse(Error("unmatchable term", NoPosition).failure)
      case Add(term) =>
        matcher.matchingTermFromTerm(term).map {
          matchingTerm =>
            val value = tabular.size(table) + 1
            tabular.add(table, matchingTerm, value).map {
              _.map {
                case (table, oldValue) => oldValue.map { v => (table, ReplacedTermResult(v)) }.getOrElse((table, AddedValueResult(value)))
              }.getOrElse((table, NotAddedValueResult))
            }
        }.getOrElse(Error("unmatchable term", NoPosition).failure)
    }
  
  def execute(instrs: List[Instruction])(table: Table[Int]) =
    instrs.foldLeft((table, List[Result]()).success[AbstractError]) {
      (res, i) => res.flatMap { case (t, iReses) => executeInstruction(i)(t).map { case (t2, iRes) => (t2, iRes :: iReses) } }
    }.map { case (t, iReses) => (t, iReses.reverse) }
    
  def executeInstructionString(s: String)(table: Table[Int]) =
    for {
      instr <- Parser.parseInstructionString(s)
      pair <- executeInstruction(instr)(table)
    } yield pair
  
  def executeString(s: String)(table: Table[Int]) =
    for {
      instrs <- Parser.parseString(s)
      pair <- execute(instrs)(table)
    } yield pair   
}

object Executor
{
  def apply[T, U[_]](matcher: Matcher[T], tabular: Tabular[U, T]): Executor = {
    val matcher1 = matcher
    val tabular1 = tabular
    new Executor {
      override type MatchingTerm = T
      override type Table[V] = U[V]
      override implicit val matcher = matcher1
      override implicit val tabular = tabular1
    }
  }
  
  def executor[T, U[_]](implicit matcher: Matcher[T], tabular: Tabular[U, T]) = apply(matcher, tabular)
}