package pl.luckboy.logicalterm
import scalaz._
import scalaz.Scalaz._

sealed trait ConcatSeq[T]
{
  def ++ (seq: ConcatSeq[T]) =
    seq match {
      case SingleConcatSeq(_)  => ListConcatSeq(List(this, seq))
      case ListConcatSeq(seqs) => ListConcatSeq(this :: seqs)
    }
  
  def foldLeft[U](z: U)(f: (U, T) => U): U =
    this match {
      case SingleConcatSeq(x)   => f(z, x)
      case ListConcatSeq(seqs)  => seqs.foldLeft(z) { (x, seq) => seq.foldLeft(x)(f) }
    }
  
  def toSeq = foldLeft(List[T]()) { (xs, x) => x :: xs }.reverse
  
  def toVector = foldLeft(Vector[T]()) { _ :+ _ }
  
  def toSet = foldLeft(Set[T]()) { _ + _ }
}

object ConcatSeq
{
  def empty[T] = ListConcatSeq(List[ConcatSeq[T]]())
  
  def apply[T](xs: T*) =
    xs.headOption.map { 
      x => if(xs.size === 1) SingleConcatSeq(x) else ListConcatSeq(xs.map { SingleConcatSeq(_) }.toList)
    }.getOrElse(ListConcatSeq(Nil))
}

case class SingleConcatSeq[T](x: T) extends ConcatSeq[T]
case class ListConcatSeq[T](seqs: List[ConcatSeq[T]]) extends ConcatSeq[T]