/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
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
  
  def toSeq: Seq[T] = toList

  def toList = foldLeft(List[T]()) { (xs, x) => x :: xs }.reverse
  
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
    
  def fromIterable[T](xs: Iterable[T]) =
    xs.headOption.map { 
      x => if(xs.size === 1) SingleConcatSeq(x) else ListConcatSeq(xs.map { SingleConcatSeq(_) }.toList)
    }.getOrElse(ListConcatSeq(Nil))  
}

case class SingleConcatSeq[T](x: T) extends ConcatSeq[T]
case class ListConcatSeq[T](seqs: List[ConcatSeq[T]]) extends ConcatSeq[T]
