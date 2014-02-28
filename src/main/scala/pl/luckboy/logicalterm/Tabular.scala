package pl.luckboy.logicalterm

trait Tabular[T, U, V]
{
  def empty: T 
  
  def find(table: T, term: U): Option[V]
  
  def add(table: T, term: U, value: V): Option[(T, Option[U])]
  
  def size(table: T): Int
}