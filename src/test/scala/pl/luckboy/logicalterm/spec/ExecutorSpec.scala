package pl.luckboy.logicalterm.spec
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.logicalterm._
import pl.luckboy.logicalterm.simple.termMatcher

class ExecutorSpec extends FlatSpec with ShouldMatchers
{
  def executor(exec: Executor)
  {
    val empty = exec.emptyTable    
    def execute1(s: String)(table: exec.Table[Int]) = exec.executeInstructionString(s)(table)
    def execute(s: String)(table: exec.Table[Int]) = exec.executeString(s)(table)
    def e1(s: String) = execute1(s)(empty).map { _._2 }
    def e(s: String) = execute(s)(empty).map { _._2 }
    
    it should "match the terms" in {
      e1("a & b = a & b") should be ===(MatchedTermResult.success)
      e1("(a | b) & c = a & c | b & c") should be ===(MatchedTermResult.success)
    }
  }
  
  "A simple Executor" should behave like executor(Executor.executor[Term, simple.TableP[Term]#A])
}