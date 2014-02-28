package pl.luckboy.logicalterm

trait Matcher[T]
{
  def matchingTermFromTerm(term: Term): T
  
  def matches(term1: T, term2: T, matching: Matching.Value): Boolean
}