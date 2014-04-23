/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.logicalterm
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.PackratParsers
import scalaz._
import scalaz.Scalaz._

object Parser extends StandardTokenParsers with PackratParsers
{
  lexical.delimiters ++= List("(", ")", "&", "|", "<=", ">=", "=")
  lexical.reserved ++= List("find", "add")
  
  lazy val expr: PackratParser[Term] = expr1
  lazy val expr1 = expr2 ~ (("|" ~> expr2) *)			^^ { case t ~ ts => if(ts.isEmpty) t else Disjunction((t :: ts).toSet) }
  lazy val expr2 = expr3 ~ (("&" ~>  expr3) *)			^^ { case t ~ ts => if(ts.isEmpty) t else Conjunction((t :: ts).toSet) }
  lazy val expr3 = varApp | expr4 
  lazy val varApp = ident ~ (expr4 +)					^^ { case s ~ ts => VarApp(s, ts.toVector) }
  lazy val expr4: PackratParser[Term] = "(" ~> expr <~ ")" | variable
  lazy val variable = ident 							^^ { case s => VarApp(s, Vector()) }
    
  lazy val instr = (
      expr ~ ("=" ~> expr)								^^ { case t1 ~ t2 => Match(t1, t2, Matching.Terms) }
      | expr ~ (">=" ~> expr)							^^ { case t1 ~ t2 => Match(t1, t2, Matching.SupertermWithTerm) }
      | expr ~ ("<=" ~> expr)							^^ { case t1 ~ t2 => Match(t1, t2, Matching.TermWithSuperterm) }
      | "find" ~> expr									^^ Find[Term]
      | "add" ~> expr									^^ Add[Term])
    
  def parseTermString(s: String): Validation[AbstractError, Term] =
    phrase(expr)(new lexical.Scanner(s)) match {
      case Success(term, _)   => term.success
      case Failure(msg, next) => pl.luckboy.logicalterm.Error(msg, next.pos).failure
      case Error(msg, next)   => pl.luckboy.logicalterm.FatalError(msg, next.pos).failure
    }
  
  def parseInstructionString(s: String): Validation[AbstractError, Instruction[Term]] =
    phrase(instr)(new lexical.Scanner(s)) match {
      case Success(instr, _)  => instr.success
      case Failure(msg, next) => pl.luckboy.logicalterm.Error(msg, next.pos).failure
      case Error(msg, next)   => pl.luckboy.logicalterm.FatalError(msg, next.pos).failure
    }

  def parseString(s: String): Validation[AbstractError, List[Instruction[Term]]] =
    s.split("\n").flatMap { t => if(t.matches("^\\s*$")) Nil else List(t) }.foldLeft(List[Instruction[Term]]().success[AbstractError]) {
      (res, line) => res.flatMap { is => parseInstructionString(line).map { _ :: is } }
    }.map { _.reverse }
}
