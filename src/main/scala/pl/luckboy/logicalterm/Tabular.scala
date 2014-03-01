package pl.luckboy.logicalterm
import scalaz._

trait Tabular[T[_], U]
{
  def empty[V]: T[V] 
  
  def find[V](table: T[V], term: U): Validation[FatalError, Validation[FindingFailure.Value, V]]
  
  def add[V](table: T[V], term: U, value: V): Validation[FatalError, Option[(T[V], Option[V])]]
  
  def size[V](table: T[V]): Int
}