package pl.luckboy.logicalterm.simple
import pl.luckboy.logicalterm._

case class Table[T, U](pairs: Seq[(T, U)])

trait TableP[T]
{
  type A[U] = Table[T, U]
}