/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.logicalterm

package object range2
{
  implicit val termNodeRangeOrdering = new Ordering[TermNodeRange] {
    override def compare(x: TermNodeRange, y: TermNodeRange) =
      if(x.maxIdx < y.minIdx ) -1 else if(x.minIdx > y.maxIdx) 1 else 0
  }
  
  implicit val matchingTermMatcher: Matcher[MatchingTerm] = new MatchingTermMatcher
  
  implicit val counterGraphLocationEqual: scalaz.Equal[CounterGraphLocation] = new scalaz.Equal[CounterGraphLocation] {
    override def equal(a1: CounterGraphLocation, a2: CounterGraphLocation) = a1 == a2
  }
}
