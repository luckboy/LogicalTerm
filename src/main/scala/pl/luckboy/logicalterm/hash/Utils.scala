package pl.luckboy.logicalterm.hash
import scala.collection.immutable.IntMap
import scalaz._
import scalaz.Scalaz._

object Utils
{
  @inline
  def matchesSuperHashCodeWithHashCode(hashCode1: Int, hashCode2: Int) = (~hashCode1 & hashCode2) === 0
  
  @inline
  def matchesHashCodes(hashCode1: Int, hashCode2: Int) = hashCode1 === hashCode2
  
  def filterTermsFromSuperterm(terms: IntMap[List[MatchingTerm]], superterm: MatchingTerm) = {
    val hashCode = superterm.lazyHashCode
    terms.flatMap { case (hc, ts) => if(matchesSuperHashCodeWithHashCode(hashCode, hc)) ts else Nil }
  }

  def filterSupertermsFromTerm(superterms: IntMap[List[MatchingTerm]], term: MatchingTerm) = {
    val hashCode = term.lazyHashCode
    superterms.flatMap { case (hc, ts) => if(matchesSuperHashCodeWithHashCode(hc, hashCode)) ts else Nil }
  }
}