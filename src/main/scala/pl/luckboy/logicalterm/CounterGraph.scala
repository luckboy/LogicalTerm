/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.logicalterm
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scalaz._
import scalaz.Scalaz._

case class CounterGraph[T](vertices: Map[T, CounterGraphVertice[T]])
{
  @tailrec
  final def decreaseCounters(q: Queue[T]): Option[CounterGraph[T]] =
    if(!q.isEmpty) {
      val (vLoc, q2) = q.dequeue
      vertices.get(vLoc) match {
        case Some(v) =>
          val optPair = v.neighborLocs.foldLeft(some((this: CounterGraph[T], q2))) {
            case (Some((g, q3)), uLoc) =>
              g.vertices.get(uLoc).map { 
                u =>
                  if(u.count > 0) 
                    (CounterGraph(g.vertices.updated(uLoc, u.copy(count = u.count - 1))), if(u.count === 1) q3.enqueue(uLoc) else q3)
                  else
                    (g, q3)
              }
            case (None, _)             =>
              none
          }
          optPair match {
            case Some((g2, q4)) => g2.decreaseCounters(q4)
            case None           => none
          }
        case None    =>
          none
      }
    } else
      some(this)
}

case class CounterGraphVertice[T](
    neighborLocs: List[T],
    count: Int)
