package pl.luckboy
import pl.luckboy.logicalterm.simple.termMatcher

package object logicalterm
{  
  val simpleExecutor = Executor.executor[Term, simple.TableP[Term]#A]
  
  val hashSimpleExecutor = Executor.executor[hash.MatchingTerm, simple.TableP[hash.MatchingTerm]#A]
}