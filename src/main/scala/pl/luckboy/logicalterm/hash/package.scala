package pl.luckboy.logicalterm

package object hash
{
  implicit val matchingTermMatcher: Matcher[MatchingTerm] = new MatchingTermMatcher
}