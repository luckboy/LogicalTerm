/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.logicalterm
import scalaz._

trait Tabular[T[_], U]
{
  def empty[V]: T[V] 
  
  def find[V](table: T[V], term: U): Validation[FatalError, Validation[FindingFailure.Value, V]]
  
  def add[V](table: T[V], term: U, value: V): Validation[FatalError, Option[(T[V], Option[V])]]
  
  def size[V](table: T[V]): Int
}
