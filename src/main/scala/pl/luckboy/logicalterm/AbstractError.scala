/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.logicalterm
import scala.util.parsing.input.Position

sealed trait AbstractError
{
  override def toString =
    this match {
      case Error(msg, pos)                  =>
        pos + ": " + msg
      case FatalError(msg, pos, stackTrace) =>
        "fatal: " + pos + ": " + msg + "\n" +
        stackTrace.map { (" " * 8) + _ }.mkString("\n") + "\n"
    }
}
case class Error(msg: String, pos: Position) extends AbstractError
case class FatalError(msg: String, pos: Position, stackTrace: List[java.lang.StackTraceElement] = Thread.currentThread().getStackTrace().toList) extends AbstractError
