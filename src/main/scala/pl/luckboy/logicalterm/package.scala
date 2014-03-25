package pl.luckboy
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.logicalterm.simple.termMatcher

package object logicalterm
{  
  implicit val termEqual = new Equal[Term] {
    override def equal(a1: Term, a2: Term) = a1 == a2
  }
  
  val simpleExecutor = Executor.executor[Term, simple.TableP[Term]#A]
  
  val hashSimpleExecutor = Executor.executor[hash.MatchingTerm, simple.TableP[hash.MatchingTerm]#A]
  
  val rangeSimpleExecutor = Executor.executor[range.MatchingTerm, simple.TableP[range.MatchingTerm]#A]
}