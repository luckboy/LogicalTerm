/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.logicalterm.simple.termMatcher

package object logicalterm
{  
  implicit val termEqual = new Equal[Term] {
    override def equal(a1: Term, a2: Term) = a1 == a2
  }
  
  val simpleExecutor = Executor.executor[Term, simple.TableP[Term]#A]
  
  val hashSimpleExecutor = Executor.executor[hash.MatchingTerm, simple.TableP[hash.MatchingTerm]#A]
  
  val rangeSimpleExecutor = Executor.executor[range.MatchingTerm, simple.TableP[range.MatchingTerm]#A]
  
  val range2SimpleExecutor = Executor.executor[range2.MatchingTerm, simple.TableP[range2.MatchingTerm]#A]
}
