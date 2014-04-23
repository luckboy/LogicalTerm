/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.logicalterm
import scalaz._

sealed trait Result
{
  override def toString =
    this match {
      case MatchedTermResult            => "matched"
      case MismatchedTermResult         => "mismatched"
      case FoundValueResult(value)      => value.toString
      case NotFoundValueResult(failure) => failure.toString
      case AddedValueResult(value)      => "added (value = " + value + ")" 
      case ReplacedTermResult(value)    => "replaced term (value = " + value + ")"
      case NotAddedValueResult          => "not added"
    }
}
case object MatchedTermResult extends Result
case object MismatchedTermResult extends Result
case class FoundValueResult(value: Int) extends Result
case class NotFoundValueResult(failure: FindingFailure.Value) extends Result
case class AddedValueResult(value: Int) extends Result
case class ReplacedTermResult(value: Int) extends Result
case object NotAddedValueResult extends Result
