package pl.luckboy.logicalterm

sealed trait Term
{
  private def toArgString: String =
    this match {
      case VarApp(_, args) if !args.isEmpty   => "(" + this + ")"
      case Conjunction(_)                     => "(" + this + ")"
      case Disjunction(_)                     => "(" + this + ")"
      case _                                  => toString
    }
  
  override def toString =
    this match {
      case VarApp(varName, args) => varName + args.map { " " + _.toArgString }.mkString
      case Conjunction(terms) =>
        terms.map {
          case term @ (Disjunction(_) | Conjunction(_)) => term.toArgString
          case term                                     => term.toString
        }.mkString(" & ")
      case Disjunction(terms) =>
        terms.map {
          case term @ (Disjunction(_) | Conjunction(_)) => term.toArgString
          case term                                     => term.toString
        }.mkString(" | ")
    }
}

case class VarApp(varName: String, args: Seq[Term]) extends Term
case class Conjunction[T, U](terms: Set[Term]) extends Term
case class Disjunction[T, U](terms: Set[Term]) extends Term