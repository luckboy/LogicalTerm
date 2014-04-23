/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.logicalterm.simple
import pl.luckboy.logicalterm._

case class Table[T, U](pairs: Seq[(T, U)])

trait TableP[T]
{
  type A[U] = Table[T, U]
}
