package pl.luckboy.logicalterm

package object simple 
{
  implicit val termMatcher = new TermMatcher
  
  implicit def tableTabular[T, U](implicit matcher: Matcher[T]) = new TableTabular[T]
}