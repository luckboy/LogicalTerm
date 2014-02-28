package pl.luckboy.logicalterm
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.PackratParsers

object Parser extends StandardTokenParsers with PackratParsers
{
  lazy val expr: PackratParser[Term] = expr1
  lazy val expr1 = expr2 ~ (("|" ~> expr2) *)			^^ { case t ~ ts => if(ts.isEmpty) t else Disjunction((t :: ts).toSet) }
  lazy val expr2 = expr3 ~ (("&" ~>  expr3) *)			^^ { case t ~ ts => if(ts.isEmpty) t else Conjunction((t :: ts).toSet) }
  lazy val expr3: PackratParser[Term] = "(" ~> expr <~ ")" | varApp
  lazy val varApp = ident ~ (expr3 *)					^^ { case s ~ ts => VarApp(s, ts.toVector) }
}