package pl.luckboy.logicalterm
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.PackratParsers
import scalaz._
import scalaz.Scalaz._

object Parser extends StandardTokenParsers with PackratParsers
{
  lexical.delimiters ++= List("(", ")", "&", "|", "=", "<=", ">=", "\n")
  lexical.reserved ++= List("match", "find", "add")
  
  lazy val expr: PackratParser[Term] = expr1
  lazy val expr1 = expr2 ~ (("|" ~> expr2) *)			^^ { case t ~ ts => if(ts.isEmpty) t else Disjunction((t :: ts).toSet) }
  lazy val expr2 = expr3 ~ (("&" ~>  expr3) *)			^^ { case t ~ ts => if(ts.isEmpty) t else Conjunction((t :: ts).toSet) }
  lazy val expr3: PackratParser[Term] = "(" ~> expr <~ ")" | varApp
  lazy val varApp = ident ~ (expr3 *)					^^ { case s ~ ts => VarApp(s, ts.toVector) }
  
  lazy val exprPair = expr3 ~ expr3						^^ { case t1 ~ t2 => (t1, t2) }
  
  lazy val instr = (
      "match" ~> expr1 ~ ("=" ~> expr1)					^^ { case t1 ~ t2 => Match(t1, t2, Matching.Terms) }
      | expr1 ~ (">=" ~> expr1)							^^ { case t1 ~ t2 => Match(t1, t2, Matching.SupertermWithTerm) }
      | expr1 ~ ("<=" ~> expr1)							^^ { case t1 ~ t2 => Match(t1, t2, Matching.TermWithSuperterm) }
      | "find" ~> expr1									^^ Find
      | "add" ~> expr1									^^ Add)
  
  lazy val instrs = instr ~ (("\n" ~> instr) *)			^^ { case i ~ is => i :: is }
  
  def parseTermString(s: String): Validation[AbstractError, Term] =
    phrase(expr)(new lexical.Scanner(s)) match {
      case Success(term, _)   => term.success
      case Failure(msg, next) => pl.luckboy.logicalterm.Error(msg, next.pos).failure
      case Error(msg, next)   => pl.luckboy.logicalterm.FatalError(msg, next.pos).failure
    }
  
  def parseInstructionString(s: String): Validation[AbstractError, Instruction] =
    phrase(instr)(new lexical.Scanner(s)) match {
      case Success(instr, _)  => instr.success
      case Failure(msg, next) => pl.luckboy.logicalterm.Error(msg, next.pos).failure
      case Error(msg, next)   => pl.luckboy.logicalterm.FatalError(msg, next.pos).failure
    }

  def parseString(s: String): Validation[AbstractError, List[Instruction]] =
    phrase(instrs)(new lexical.Scanner(s)) match {
      case Success(instrs, _)  => instrs.success
      case Failure(msg, next) => pl.luckboy.logicalterm.Error(msg, next.pos).failure
      case Error(msg, next)   => pl.luckboy.logicalterm.FatalError(msg, next.pos).failure
    }
}