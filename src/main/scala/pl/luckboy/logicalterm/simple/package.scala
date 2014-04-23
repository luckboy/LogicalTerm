/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.logicalterm

package object simple 
{
  implicit val termMatcher: Matcher[Term] = new TermMatcher
  
  implicit def tableTabular[T](implicit matcher: Matcher[T]): Tabular[TableP[T]#A, T] = new TableTabular[T]
}
