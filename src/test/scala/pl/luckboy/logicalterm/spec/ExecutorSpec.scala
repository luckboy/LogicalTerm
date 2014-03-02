package pl.luckboy.logicalterm.spec
import org.scalatest.FlatSpec
import org.scalatest.Inside
import org.scalatest.matchers.ShouldMatchers
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.logicalterm._

class ExecutorSpec extends FlatSpec with ShouldMatchers with Inside
{
  def executor(exec: Executor)
  {
    val emptyTable = exec.emptyTable    
    def execute1(s: String, table: exec.Table[Int] = emptyTable) = exec.executeInstructionString(s)(table)
    def execute(s: String, table: exec.Table[Int] = emptyTable) = exec.executeString(s)(table)
    def e1(s: String, table: exec.Table[Int] = emptyTable) = execute1(s, table).map { _._2 }
    def e(s: String, table: exec.Table[Int] = emptyTable) = execute(s, table).map { _._2 }
    
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
    
    it should "not match the terms" in {
      e1("a & b | c = a | b & c") should be ===(MismatchedTermResult.success)
      e1("a & c = a & b & c") should be ===(MismatchedTermResult.success)
      e1("(a1 & a2) | (b1 & b2) | c1 = a1 & (b1 | b2) & (c1 | c2)") should be ===(MismatchedTermResult.success)
      e1("a & (b1 | b2) & (c1 | c2) = a | (b1 & b2 & b3) | c | d") should be ===(MismatchedTermResult.success)
    }
    
    it should "not match the terms as superterm with term" in {
      e1("(a1 & a2) | b | (c1 & c2) >= a1 | b | c1") should be ===(MismatchedTermResult.success)
      e1("(a1 | a2) & b & (c1 | c2) >= (a1 | a3) & b & (c1 | c2 | c3)") should be ===(MismatchedTermResult.success)
    }
    
    it should "not match the terms as term with superterm" in {
      e1("a | (b1 & b2) | c | d <= a | (b1 & b2 & b3) | d") should be ===(MismatchedTermResult.success)
      e1("(a1 | a2) & (b1 | b2) & c <= a1 & (b1 | b3) & c") should be ===(MismatchedTermResult.success)
    }
    
    it should "not match the terms these have the different variables" in {
      e1("a & b & c = d & e") should be ===(MismatchedTermResult.success)
      e1("a | b | c = d | e | f") should be ===(MismatchedTermResult.success)
      e1("a & b & c >= d & e") should be ===(MismatchedTermResult.success)
      e1("a | b | c <= d | e | f") should be ===(MismatchedTermResult.success)
    }

    it should "not match the variable applications" in {
      e1("a (b | c) b (d & c) = a (b & c) b d") should be ===(MismatchedTermResult.success)
    }
    
    it should "add a value with the term" in {
      e1("add a & b") should be ===(AddedValueResult(1).success)
      e1("add a & b & c") should be ===(AddedValueResult(1).success)
    }

    it should "add values with the terms" in {
      e("""
add a & b
add c
add d | e
add (a & f) | g 
""") should be ===(List(
        AddedValueResult(1),
        AddedValueResult(2),
        AddedValueResult(3),
        AddedValueResult(4)).success)
    }
    
    it should "find the value for the terms" in {
      inside(execute("""
add a & b
add c
add d | e
""")) {
        case Success((table, _)) =>
          e1("find a & b", table) should be ===(FoundValueResult(1).success)
          e1("find d | e | f", table) should be ===(FoundValueResult(3).success)
      }
    }
    
    it should "add values with the terms these aren't superterms for themselves" in {
      e("""
add (a1 & a2) | b1 | (c1 & (c21 | c22) & c3)
add a1 | (b1 & b2) | c21 | c22
add c3 & (d1 | d2)
""") should be ===(List(
        AddedValueResult(1),
        AddedValueResult(2),
        AddedValueResult(3)).success)
    }
    
    it should "replace the terms these are superterms or equaled for themselves" in {
       e("""
add (a1 & a2) | b1 | (c1 & (c21 | c22) & c3)
add a1 | (b1 & b2) | c21 | c22
add (a1 & a2 & a3) | b1 | (c1 & c22 & c3)
add a1 | (b1 & b2) | c21 | c22 | (d1 & d2)
add (e1 | e2) & g1 & (h1 | (h21 & h22))
add (e1 | e2) & g1 & ((h1 | h21) & (h1 | h22))
add (e1 | e2) & (g1 | g2) & (h1 | h21)
""") should be ===(List(
        AddedValueResult(1),
        AddedValueResult(2),
        ReplacedTermResult(1),
        ReplacedTermResult(2),
        AddedValueResult(3),
        ReplacedTermResult(3),
        ReplacedTermResult(3)).success)
    }
  }
  
  "A simpleExecutor" should behave like executor(simpleExecutor)
}