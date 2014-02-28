package pl.luckboy.logicalterm
import scalaz._

trait Tabular[T, U, V]
{
  def empty: T 
  
  def find(table: T, term: U): Validation[FatalError, Option[V]]
  
  def add(table: T, term: U, value: V): Validation[FatalError, Option[(T, Option[U])]]
  
  def size(table: T): Int
}