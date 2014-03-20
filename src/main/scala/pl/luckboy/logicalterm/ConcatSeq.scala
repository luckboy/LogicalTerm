package pl.luckboy.logicalterm

sealed trait ConcatSeq[T]
{
  def ++ (seq: ConcatSeq[T]) =
    seq match {
      case SingleConcatSeq(_)  => ListConcatSeq(this :: seq :: Nil)
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
}

case class SingleConcatSeq[T](x: T) extends ConcatSeq[T]
case class ListConcatSeq[T](seqs: List[ConcatSeq[T]]) extends ConcatSeq[T]