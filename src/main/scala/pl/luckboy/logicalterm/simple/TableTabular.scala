/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.logicalterm.simple
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.logicalterm._

class TableTabular[T](implicit matcher: Matcher[T]) extends Tabular[TableP[T]#A, T]
{
  override def empty[U] = Table(Vector[(T, U)]())
  
  private def findValuesWithIndexes[U](table: Table[T, U], term: T, matching: Matching.Value) =
    table.pairs.zipWithIndex.foldLeft(Vector[(U, Int)]().success[FatalError]) {
      case (Success(ps), ((t, v), i)) => matcher.matches(term, t, matching).map { if(_) ps :+ (v, i) else ps }
      case (Failure(err), _)          => err.failure
    }
  
  override def find[U](table: Table[T, U], term: T) =
    findValuesWithIndexes(table, term, Matching.SupertermWithTerm).map {
      _ match {
        case Seq((value, _)) => value.success
        case Seq()           => FindingFailure.NotFound.failure
        case _               => FindingFailure.TooMany.failure
      }
    }
  
  override def add[U](table: Table[T, U], term: T, value: U): Validation[FatalError, Option[(Table[T, U], Option[U])]] = {
    val supertermPairListRes = findValuesWithIndexes(table, term, Matching.TermWithSuperterm)
    val subtermPairListRes = findValuesWithIndexes(table, term, Matching.SupertermWithTerm)
    (for(ps1 <- supertermPairListRes; ps2 <- subtermPairListRes) yield (ps1, ps2)).map {
      _ match {
        case (Seq(), Seq())                      =>
          some((table.copy(pairs = table.pairs :+ (term, value)), none))
        case (Seq((oldValue, i)), Seq())         =>
          some((table.copy(pairs = table.pairs.updated(i, (term, oldValue))), some(oldValue)))
        case (Seq(), Seq((oldValue, i)))         =>
          some((table, some(oldValue)))
        case (Seq((oldValue, i1)), Seq((_, i2))) =>
          if(i1 === i2)
            some((table.copy(pairs = table.pairs.updated(i1, (term, oldValue))), some(oldValue)))
          else
            none
        case _                                   =>
          none
      }
    }
  }
  
  override def size[U](table: Table[T, U]) = table.pairs.size
}
