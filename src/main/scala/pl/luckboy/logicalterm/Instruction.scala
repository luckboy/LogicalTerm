/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.logicalterm

sealed trait Instruction[+T]
case class Match[+T](term1: T, term2: T, matching: Matching.Value) extends Instruction[T]
case class Find[+T](term: T) extends Instruction[T]
case class Add[+T](term: T) extends Instruction[T]
