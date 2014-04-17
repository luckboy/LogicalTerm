package pl.luckboy.logicalterm

package object range2
{
  implicit val termNodeRangeOrdering = new Ordering[TermNodeRange] {
    override def compare(x: TermNodeRange, y: TermNodeRange) =
      if(x.maxIdx < y.minIdx ) -1 else if(x.minIdx > y.maxIdx) 1 else 0
  }
  
  implicit val matchingTermMatcher: Matcher[MatchingTerm] = new MatchingTermMatcher
}