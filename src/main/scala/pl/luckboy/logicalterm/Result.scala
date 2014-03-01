package pl.luckboy.logicalterm
import scalaz._

sealed trait Result
{
  override def toString =
    this match {
      case MatchedTermResult            => "matched"
      case MismatchedTermResult         => "mismatched"
      case FoundValueResult(value)      => value.toString
      case NotFoundValueResult(failure) => failure.toString
      case AddedValueResult             => "added"
      case ReplacedTermResult(value)    => "replaced term (value = " + value + ")"
      case NotAddedValueResult          => "not added"
    }
}
case object MatchedTermResult extends Result
case object MismatchedTermResult extends Result
case class FoundValueResult(value: Int) extends Result
case class NotFoundValueResult(failure: FindingFailure.Value) extends Result
case object AddedValueResult extends Result
case class ReplacedTermResult(value: Int) extends Result
case object NotAddedValueResult extends Result