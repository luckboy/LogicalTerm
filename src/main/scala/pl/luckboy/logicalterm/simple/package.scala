package pl.luckboy.logicalterm

package object simple 
{
  val termMatcher = new TermMatcher
  
  def tableTabular[T, U](implicit matcher: Matcher[T]) = new TableTabular[T, U]
}