package pl.luckboy.logicalterm

package object simple 
{
  implicit val termMatcher: Matcher[Term] = new TermMatcher
  
  implicit def tableTabular[T](implicit matcher: Matcher[T]): Tabular[TableP[T]#A, T] = new TableTabular[T]
}