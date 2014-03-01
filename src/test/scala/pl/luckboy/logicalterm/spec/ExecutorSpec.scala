package pl.luckboy.logicalterm.spec
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.logicalterm._

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
    
    it should "match the conjunctions" in {
      e1("a & b = a & b") should be ===(MatchedTermResult.success)
      e1("a & b & c = a & b & c") should be ===(MatchedTermResult.success)
    }
    
    it should "match the disjunctions" in {
      e1("a | b = a | b") should be === (MatchedTermResult.success)
      e1("a | b | c = a | b | c") should be === (MatchedTermResult.success)
    }
    
    it should "match the same terms" in {
      e1("(a1 & a2) | b | (c1 & (c21 | c22) & c3) = (a1  & a2) | b | (c1 & (c21 | c22) & c3)") should be ===(MatchedTermResult.success)
      e1("(a1 | a2) & (b1 | (b21 & b22) | (b31 | b32)) & c = (a1 | a2) & (b1 | (b21 & b22) | (b31 | b32)) & c") should be ===(MatchedTermResult.success)
    }
    
    it should "match the different terms" in {
      e1("((a1 | a2) & b & c1) | ((a1 | a2) & b & (c2 | c3)) = (a1 | a2) & b & (c1 | c2 | c3)") should be ===(MatchedTermResult.success)
      e1("a | (b1 & b2) | (c1 | (d1 & d2)) = (a | b1 | (c1 | (d1 & d2))) & (a | b2 | (c1 | (d1 & d2)))") should be ===(MatchedTermResult.success)
    }
    
    it should "match the variable applications" in {
      e1("a (b | c & e) (b & d) = a ((b | c) & (b | e)) (b & d)") should be ===(MatchedTermResult.success)
    }
    
    it should "match the conjunctions as superterm with term" in {
      e1("a & b >= a & b") should be ===(MatchedTermResult.success)
      e1("a & b >= a & b & c") should be ===(MatchedTermResult.success)
      e1("a & b & c >= a & b & c & d & e") should be ===(MatchedTermResult.success)
    }
    
    it should "match the disjunctions as superterm with term" in {
      e1("a | b >= a | b") should be ===(MatchedTermResult.success)
      e1("a | b | c >= a | b") should be ===(MatchedTermResult.success)
      e1("a | b | c | d | e >= a | b | c") should be ===(MatchedTermResult.success)
    }
    
    it should "match the same terms as superterm with term" in {
      e1("(a1 & a2) | (b1 & (b21 | b22) & b3) | c >= (a1 & a2) | (b1 & (b21 | b22) & b3) | c") should be ===(MatchedTermResult.success)
      e1("(a1 | a2 | a3) & (b1 | b2) & (c1 | c2) >= (a1 | a2 | a3) & (b1 | b2) & (c1 | c2)") should be ===(MatchedTermResult.success)
    }
    
    it should "match the different terms as superterm with term" in {
      e1("((a1 | a2) & b & c1) | ((a1 | a2) & b & (c2 | c3)) >= (a1 | a2) & b & (c1 | c2 | c3)") should be ===(MatchedTermResult.success)
      e1("a | (b1 & b2) | (c1 | (d1 & d2)) >= (a | b1 | (c1 | (d1 & d2))) & (a | b2 | (c1 | (d1 & d2)))") should be ===(MatchedTermResult.success)
      e1("(a1 | (b1 & b3) | c) & (a2 | b21 | b22 | c | d) >= (a1 & a2) | (b1 & (b21 | b22) & b3) | c") should be ===(MatchedTermResult.success)
      e1("a & (b1 | b2 | b3) & (c1 | c2) >= (a & b1 & c1 & d) | (a & b2 & (c1 | c2))") should be ===(MatchedTermResult.success)
    }
    
    it should "match the variable applications as superterm with term" in {
      e1("a (b1 | b2) c (d1 & d2) >= a (b1 | b2) c (d1 & d2)") should be ===(MatchedTermResult.success)
    }
    
    it should "match the conjunctions as term with superterm" in {
      e1("a & b <= a & b") should be ===(MatchedTermResult.success)
      e1("a & b & c <= a & b") should be ===(MatchedTermResult.success)
      e1("a & b & c & d & e <= a & b & c") should be ===(MatchedTermResult.success)
    }
    
    it should "match the disjunctions as term with superterm" in {
      e1("a | b <= a | b") should be ===(MatchedTermResult.success)
      e1("a | b <= a | b | c") should be ===(MatchedTermResult.success)
      e1("a | b | c <= a | b | c | d | e") should be ===(MatchedTermResult.success)
    }
    
    it should "match the same terms as term with superterm" in {
      e1("a | (b1 & (b21 | b22 | b23)) | (c1 | c2 | c3) <= a | (b1 & (b21 | b22 | b23)) | (c1 | c2 | c3)") should be ===(MatchedTermResult.success)
      e1("(a1 | (a21 & a22)) | b | (c1 | c2 | c3) <= (a1 | (a21 & a22)) | b | (c1 | c2 | c3)") should be ===(MatchedTermResult.success)
    }
    
    it should "match the different terms as term with superterm" in {
      e1("((a1 | a2) & b & c1) | ((a1 | a2) & b & (c2 | c3)) <= (a1 | a2) & b & (c1 | c2 | c3)") should be ===(MatchedTermResult.success)
      e1("a | (b1 & b2) | (c1 | (d1 & d2)) <= (a | b1 | (c1 | (d1 & d2))) & (a | b2 | (c1 | (d1 & d2)))") should be ===(MatchedTermResult.success)
      e1("(b1 | (c1 & c2 & c3)) & (a | b21 | (c1 & c2)) & d <= a | (b1 & (b21 | b22)) | (c1 & c2)") should be ===(MatchedTermResult.success)
      e1("(a1 | a2) & b & (c1 | c2 | c3) <= (a1 & (c1 | c2 | c3)) | ((a2 | a3) & b & (c1 | c2 | c3))") should be ===(MatchedTermResult.success)
    }
    
    it should "match the variable applications as term with superterm" in {
      e1("a (b1 & (b21 | b22)) c (d1 | d2) <= a ((b1 & b21) | (b1 & b22)) c (d1 | d2)") should be ===(MatchedTermResult.success)
    }
  }
  
  "A simpleExecutor" should behave like executor(simpleExecutor)
}