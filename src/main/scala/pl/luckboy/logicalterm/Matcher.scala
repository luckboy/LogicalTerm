package pl.luckboy.logicalterm
import scalaz._

trait Matcher[T]
{
  def matchingTermFromTerm(term: Term): T
  
  def matches(term1: T, term2: T, matching: Matching.Value): Validation[FatalError, Boolean]
}