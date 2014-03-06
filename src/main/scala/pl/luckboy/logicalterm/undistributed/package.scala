package pl.luckboy.logicalterm

package object undistributed
{
  implicit val termNodeRangeOrdering: Ordering[TermNodeRange] = new Ordering[TermNodeRange] {
    override def compare(x: TermNodeRange, y: TermNodeRange) =
      if(x.maxIdx < y.minIdx ) -1 else if(x.minIdx > y.maxIdx) 1 else 0
  }
}