/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.logicalterm
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._

abstract class Executor
{
  type MatchingTerm

  type Table[T]
  
  def matcher: Matcher[MatchingTerm]
  
  def tabular: Tabular[Table, MatchingTerm]
  
  def emptyTable = tabular.empty[Int]
  
  def matchingTermFromTerm(term: Term) = matcher.matchingTermFromTerm(term).toSuccess(FatalError("unmatchable term", NoPosition))
  
  def matchingInstructionFromInstruction(instr: Instruction[Term]) =
    instr match {
      case Match(term1, term2, matching) =>
        for(t1 <- matchingTermFromTerm(term1); t2 <- matchingTermFromTerm(term2)) yield { Match(t1, t2, matching) }
      case Find(term)                    =>
        matchingTermFromTerm(term).map { Find(_) }
      case Add(term)                     =>
        matchingTermFromTerm(term).map { Add(_) }
    }

  def matchingInstructionsFromInstructions(instrs: List[Instruction[Term]]) =
    instrs.foldLeft(List[Instruction[MatchingTerm]]().success[AbstractError]) {
      (res, i) => res.flatMap { is => matchingInstructionFromInstruction(i).map { _ :: is } }
    }.map { _.reverse }
  
  def executeMatchingInstruction(instr: Instruction[MatchingTerm])(table: Table[Int]) =
    instr match {
      case Match(term1, term2, matching) =>
        matcher.matches(term1, term2, matching).map {
          b => (table, if(b) MatchedTermResult else MismatchedTermResult)
        }
      case Find(term) =>
        tabular.find(table, term).map {
          res => (table, res.map { FoundValueResult(_) }.valueOr { NotFoundValueResult(_) })
        }
      case Add(term) =>
        val value = tabular.size(table) + 1
        tabular.add(table, term, value).map {
          _.map {
            case (table, oldValue) => oldValue.map { v => (table, ReplacedTermResult(v)) }.getOrElse((table, AddedValueResult(value)))
          }.getOrElse((table, NotAddedValueResult))
        }
    }
  
  def executeMatchingInstructions(instrs: List[Instruction[MatchingTerm]])(table: Table[Int]) =
    instrs.foldLeft((table, List[Result]()).success[AbstractError]) {
      (res, i) => res.flatMap { case (t, iReses) => executeMatchingInstruction(i)(t).map { case (t2, iRes) => (t2, iRes :: iReses) } }
    }.map { case (t, iReses) => (t, iReses.reverse) }
  
  def executeInstruction(instr: Instruction[Term])(table: Table[Int]) =
    for {
      instr2 <- matchingInstructionFromInstruction(instr)
      pair <- executeMatchingInstruction(instr2)(table)
    } yield pair
  
  def execute(instrs: List[Instruction[Term]])(table: Table[Int]) =
    for {
      instrs2 <- matchingInstructionsFromInstructions(instrs)
      pair <- executeMatchingInstructions(instrs2)(table)
    } yield pair
    
  def executeInstructionString(s: String)(table: Table[Int]) =
    for {
      instr <- Parser.parseInstructionString(s)
      pair <- executeInstruction(instr)(table)
    } yield pair
  
  def executeString(s: String)(table: Table[Int]) =
    for {
      instrs <- Parser.parseString(s)
      pair <- execute(instrs)(table)
    } yield pair   
}

object Executor
{
  def apply[T, U[_]](matcher: Matcher[T], tabular: Tabular[U, T]): Executor = {
    val matcher1 = matcher
    val tabular1 = tabular
    new Executor {
      override type MatchingTerm = T
      override type Table[V] = U[V]
      override implicit val matcher = matcher1
      override implicit val tabular = tabular1
    }
  }
  
  def executor[T, U[_]](implicit matcher: Matcher[T], tabular: Tabular[U, T]) = apply(matcher, tabular)
}
