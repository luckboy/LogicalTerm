/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
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
      e1("(((a1 | a2) & b) | (c & d)) & e = ((a1 | a2) & b & e) | (c & d & e)") should be ===(MatchedTermResult.success)
      e1("((a | b) & ((c1 & c2) | d)) | e = (a | b | e) & ((c1 & c2) | d | e)") should be ===(MatchedTermResult.success)
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
      e1("(((a1 | a2) & b) | (c & d)) & e >= ((a1 | a2) & b & e) | (c & d & e)") should be ===(MatchedTermResult.success)
      e1("((a | b) & ((c1 & c2) | d)) | e >= (a | b | e) & ((c1 & c2) | d | e)") should be ===(MatchedTermResult.success)
      e1("(((a1 | a2) & b) | (c & d)) & e >= (a1 & b & e) | (c & d & e & f)") should be ===(MatchedTermResult.success)
      e1("(a | b | e | f) & (c1 | d | e) >= ((a | b) & ((c1 & c2) | d)) | e") should be ===(MatchedTermResult.success)
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
      e1("(((a1 | a2) & b) | (c & d)) & e <= ((a1 | a2) & b & e) | (c & d & e)") should be ===(MatchedTermResult.success)
      e1("((a | b) & ((c1 & c2) | d)) | e <= (a | b | e) & ((c1 & c2) | d | e)") should be ===(MatchedTermResult.success)
      e1("((a1 & b) | (c & d & f)) & e <= ((a1 | a2) & b & e ) | (c & d & e)") should be ===(MatchedTermResult.success)
      e1("(a | b | e | f) & ((c1 & c2) | d | e) <= ((a | b | f) & (c1 | d)) | e") should be ===(MatchedTermResult.success)
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
    
    it should "replace the terms these are superterms or equal for themselves" in {
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
    
    it should "add values with the terms these haven't shared variables" in {
e("""
add (a1 | a2) & (b1 | (b21 & b22))
add c | (d1 & d2) | e
add (f1 | f2 | f3) & (g1 | g2) & h
""") should be ===(List(
        AddedValueResult(1),
        AddedValueResult(2),
        AddedValueResult(3)).success)
    }
    
    it should "find the value for the equal term" in {
      inside(execute("""
add a | (b1 & b2 & b3) | (c1 & (c21 | c22))
add (d1 | d2 | d3) & e
""")) {
        case Success((table, _)) =>
          e1("find a | (b1 & b2 & b3) | (c1 & (c21 | c22))", table) should be ===(FoundValueResult(1).success)
          e1("find (a | b1 | (c1 & (c21 | c22))) & (a | (b2  & b3) | (c1 & (c21 | c22)))", table) should be ===(FoundValueResult(1).success)
          e1("find (d1 & e) | ((d2 | d3) & e)", table) should be ===(FoundValueResult(2).success)
          e1("find (d1 | d2 | d3) & e", table) should be ===(FoundValueResult(2).success)
      }
    }
    
    it should "find the value for the superterm" in {
      inside(execute("""
add (a1 & a2) | b | (c1 & (c21 | c22) & c3)
add ((d1 & d2) | (e1 & e2)) & f
""")) {
        case Success((table, _)) =>
          e1("find (a1 | b | (c1 & c21 & c3) | (c1 & c22)) & (a2 | b | (c1 & (c21 | c22)))", table) should be ===(FoundValueResult(1).success)
          e1("find a2 | b | ((c21 | c22) & c3)", table) should be ===(FoundValueResult(1).success)
          e1("find (d1 | e1) & f", table) should be ===(FoundValueResult(2).success)
          e1("find (d2 & f) | (e1 & f)", table) should be ===(FoundValueResult(2).success)
      }
    }
        
    it should "add values with the variable applications" in {
      e("""
add a1 (b & c) d (b & (d | e))
add a1 (b & c) b (c & f)
add a2 (g | h) (b & (c1 | c2) & d)
add a2 (g | h | d) (b & c1)
""") should be ===(List(
        AddedValueResult(1),
        AddedValueResult(2),
        AddedValueResult(3),
        AddedValueResult(4)).success)
    }
    
    it should "find the value for the variable application" in {
      inside(execute("""
add a1 (b & c) d (b & (d | e))
add a1 (b & c) b (c & f)
add a2 (g | h) (b & (c1 | c2) & d)
add a2 (g | h | d) (b & c1)
add a3
""")) {
        case Success((table, _)) =>
          e1("find a1 (b & c) d ((b & d) | (b & e))", table) should be ===(FoundValueResult(1).success)
          e1("find a1 (b & c) b (c & f)", table) should be ===(FoundValueResult(2).success)
          e1("find a2 (g | h) ((b & c1 & d) | (b & c2 & d))", table) should be ===(FoundValueResult(3).success)
          e1("find a2 (g | h | d) (b & c1)", table) should be ===(FoundValueResult(4).success)
      }
    }
    
    it should "not find a value at the empty table" in {
      e1("find a & b") should be ===(NotFoundValueResult(FindingFailure.NotFound).success)
      e1("find a | b | c") should be ===(NotFoundValueResult(FindingFailure.NotFound).success)
    }
    
    it should "not find a value for the term" in {
      inside(execute("""
add a1 | b | (c1 & c2 & (c31 | c32))
add a2 | b | ((c31 | c33) & c4)
""")) {
        case Success((table, _)) =>
          e1("find a3 | b | c5", table) should be ===(NotFoundValueResult(FindingFailure.NotFound).success)
          e1("find (c31 | c33 | c34) & c5", table) should be ===(NotFoundValueResult(FindingFailure.NotFound).success)
      }
    }
    
    it should "not find a value for the subterm" in {
      inside(execute("""
add (a1 & a2) | b | (c1 & c2 & (c31 | c32))
add (d1 | (d21 & d22)) & e & (f1 | f2)
""")) {
        case Success((table, _)) =>
          e1("find (a1 & a2 & a3) | (c1 & c2 & c32)", table) should be ===(NotFoundValueResult(FindingFailure.NotFound).success)
          e1("find ((d1 | (d21 & d22)) & e & f1) | ((d1 | (d21 & d22)) & e & f2 & f3)", table) should be ===(NotFoundValueResult(FindingFailure.NotFound).success)
      }
    }
    
    it should "not find a value because too many terms are matched to the term of a value" in {
      inside(execute("""
add (a1 & a2) | b | c
add (a1 | b | c) & (a3 | b | c) 
add (a2 & a4) | b | c
add d & (e1 | e2)
add (d & e3) | (d & e4)
""")) {
        case Success((table, _)) =>
          e1("find a1 | b | c", table) should be ===(NotFoundValueResult(FindingFailure.TooMany).success)
          e1("find a2 | b | c", table) should be ===(NotFoundValueResult(FindingFailure.TooMany).success)
          e1("find d", table) should be ===(NotFoundValueResult(FindingFailure.TooMany).success)
      }
    }
    
    it should "not add a value because too many terms are matched to the term of a value" in {
      inside(execute("""
add (a1 & a2) | b | c
add (a1 | b | c) & (a3 | b | c)
add (a2 & a4) | b | c
add d & (e1 | e2)
add (d & e1) | (d & e3)
add d & (e2 | e4)
""")) {
        case Success((table, _)) =>
          e1("add a1 | b | c", table) should be ===(NotAddedValueResult.success)
          e1("add a2 | b | c", table) should be ===(NotAddedValueResult.success)
          e1("add d & e1", table) should be ===(NotAddedValueResult.success)
          e1("add d & e2", table) should be ===(NotAddedValueResult.success)
      }
    }
    
    it should "not find a value for the variable application" in {
       inside(execute("""
add a1 (b & c) d (b & (d | e))
add a1 (b & c) b (c & f)
add a2 (g | h) (b & (c1 | c2) & d)
add a2 (g | h | d) (b & c1)
add a3
""")) {
        case Success((table, _)) =>
          e1("find a1 (b & d) d ((b & c) | (b & e))", table) should be ===(NotFoundValueResult(FindingFailure.NotFound).success)
          e1("find a1 (b & c) d (c & f)", table) should be ===(NotFoundValueResult(FindingFailure.NotFound).success)
          e1("find a2 (g | h) ((b & c1 & d) | (b & c2))", table) should be ===(NotFoundValueResult(FindingFailure.NotFound).success)
          e1("find a2 (g | h) (b & c1)", table) should be ===(NotFoundValueResult(FindingFailure.NotFound).success)
      }
    }
    
    it should "replace the variable application" in {
       inside(execute("""
add a1 (b & c) d (b | d)
""")) {
        case Success((table, _)) =>
          e1("add a1 (b & c) d (b | d)", table) should be ===(ReplacedTermResult(1).success)
      }
    }
    
    it should "match the terms for the fixed range bug" in {
      e1("(((a1 | a2) & b) | (c & d)) & e = ((a1 | a2) & b & e) | (c & d & e)") should be ===(MatchedTermResult.success)
    }
  }
  
  def executorForDistributive(exec: Executor)
  {
    val emptyTable = exec.emptyTable    
    def execute1(s: String, table: exec.Table[Int] = emptyTable) = exec.executeInstructionString(s)(table)
    def execute(s: String, table: exec.Table[Int] = emptyTable) = exec.executeString(s)(table)
    def e1(s: String, table: exec.Table[Int] = emptyTable) = execute1(s, table).map { _._2 }
    def e(s: String, table: exec.Table[Int] = emptyTable) = execute(s, table).map { _._2 }

    it should "match the different terms (distribitive)" in {
      e1("((a1 | a2) & b & c1) | ((a1 | a2) & b & (c2 | c3)) = (a1 | a2) & b & (c1 | c2 | c3)") should be ===(MatchedTermResult.success)
      e1("a | (b1 & b2) | (c1 | (d1 & d2)) = (a | b1 | (c1 | (d1 & d2))) & (a | b2 | (c1 | (d1 & d2)))") should be ===(MatchedTermResult.success)
    }
    
    it should "match the different terms as superterm with term (distribitive)" in {
      e1("((a1 | a2) & b & c1) | ((a1 | a2) & b & (c2 | c3)) >= (a1 | a2) & b & (c1 | c2 | c3)") should be ===(MatchedTermResult.success)
      e1("a | (b1 & b2) | (c1 | (d1 & d2)) >= (a | b1 | (c1 | (d1 & d2))) & (a | b2 | (c1 | (d1 & d2)))") should be ===(MatchedTermResult.success)
      e1("(a1 | (b1 & b3) | c) & (a2 | b21 | b22 | c | d) >= (a1 & a2) | (b1 & (b21 | b22) & b3) | c") should be ===(MatchedTermResult.success)
      e1("a & (b1 | b2 | b3) & (c1 | c2) >= (a & b1 & c1 & d) | (a & b2 & (c1 | c2))") should be ===(MatchedTermResult.success)
    }
    
    it should "match the different terms as term with superterm (distributive)" in {
      e1("((a1 | a2) & b & c1) | ((a1 | a2) & b & (c2 | c3)) <= (a1 | a2) & b & (c1 | c2 | c3)") should be ===(MatchedTermResult.success)
      e1("a | (b1 & b2) | (c1 | (d1 & d2)) <= (a | b1 | (c1 | (d1 & d2))) & (a | b2 | (c1 | (d1 & d2)))") should be ===(MatchedTermResult.success)
      e1("(b1 | (c1 & c2 & c3)) & (a | b21 | (c1 & c2)) & d <= a | (b1 & (b21 | b22)) | (c1 & c2)") should be ===(MatchedTermResult.success)
      e1("(a1 | a2) & b & (c1 | c2 | c3) <= (a1 & (c1 | c2 | c3)) | ((a2 | a3) & b & (c1 | c2 | c3))") should be ===(MatchedTermResult.success)
    }
    
    it should "find the value for the equal term (distributive)" in {
      inside(execute("""
add a | (b1 & b2 & b3) | (c1 & (c21 | c22))
add (d1 | d2 | d3) & e & (f1 | (f21 | f22) | f3)
""")) {
        case Success((table, _)) =>
          e1("find a | (b1 & b2 & b3) | (c1 & (c21 | c22))", table) should be ===(FoundValueResult(1).success)
          e1("find (a | b1 | ((c1 & c21) | (c1 & c22))) & (a | (b2  & b3) | (c1 & (c21 | c22)))", table) should be ===(FoundValueResult(1).success)
          e1("find (d1 & e & (f1 | (f21 | f22) | f3)) | ((d2 | d3) & e & (f1 | (f21 | f22) | f3))", table) should be ===(FoundValueResult(2).success)
          e1("find (d1 | d2 | d3) & e & (f1 | (f21 | f22) | f3)", table) should be ===(FoundValueResult(2).success)
      }
    }
    
    it should "find the value for the superterm (distributive)" in {
      inside(execute("""
add (a1 & a2) | b | (c1 & (c21 | c22) & c3)
add (d1 | d2) & (e1 | (e21 & e22)) & (f1 | f2)
""")) {
        case Success((table, _)) =>
          e1("find (a1 | b | (c1 & c21 & c3) | (c1 & c22)) & (a2 | b | (c1 & (c21 | c22)))", table) should be ===(FoundValueResult(1).success)
          e1("find a2 | b | ((c21 | c22) & c3)", table) should be ===(FoundValueResult(1).success)
          e1("find (e1 | (e21 & e22)) & (f1 | f2)", table) should be ===(FoundValueResult(2).success)
          e1("find ((d1 | d2 | d3) & e1 & (f1 | f2)) | ((d1 | d2) & e21 & e22 & (f1 | f2 | f3))", table) should be ===(FoundValueResult(2).success)
      }
    }
  }
  
  "A simpleExecutor" should behave like executor(simpleExecutor)
  it should behave like executorForDistributive(simpleExecutor)
  
  "A hashSimpleExecutor" should behave like executor(hashSimpleExecutor)
  it should behave like executorForDistributive(hashSimpleExecutor)
  
  "A rangeSimpleExecutor" should behave like executor(rangeSimpleExecutor)
  
  "A range2SimpleExecutor" should behave like executor(range2SimpleExecutor)
  it should behave like executorForDistributive(range2SimpleExecutor)
  
  "A range3SimpleExecutor" should behave like executor(range3SimpleExecutor)
  it should behave like executorForDistributive(range3SimpleExecutor)
  
  "A range4SimpleExecutor" should behave like executor(range4SimpleExecutor)
  it should behave like executorForDistributive(range4SimpleExecutor)
}
