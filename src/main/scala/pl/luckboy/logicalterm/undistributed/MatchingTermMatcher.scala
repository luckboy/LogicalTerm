package pl.luckboy.logicalterm.undistributed
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.logicalterm._

class MatchingTermMatcher extends Matcher[MatchingTerm]
{
  override def matchingTermFromTerm(term: Term): Option[MatchingTerm] =
    throw new UnsupportedOperationException

  private def matchesTermNodes(node1: TermNode, term1: MatchingTerm, node2: TermNode, term2: MatchingTerm): Boolean = {
    // node1 is superconjunction node or disjunction node
    // node2 is superdisjunction node or conjunction node
    (node1, node2) match {
      case (TermNode(childs1, firstVarName1), TermNode(childs2, _)) if childs1.isEmpty =>
        term2.varIdxs.get(firstVarName1) match {
          case Some(varIdxs2) => 
            varIdxs2.foldLeft(true) {
              (b, varIdx2) => b & childs2.contains(TermNodeRange(varIdx2, varIdx2))
            }
          case None           =>
            false
        }
      case (TermNode(childs1, _), TermNode(childs2, _)) if (!childs1.isEmpty) && (!childs2.isEmpty) =>
        childs1.values.foldLeft(true) {
          case (true, child1) =>
            term2.varIdxs.get(child1.firstVarName) match {
              case Some(varIdxs2) =>
                varIdxs2.foldLeft(true) {
                  case (true, varIdx2) =>
                    childs2.get(TermNodeRange(varIdx2, varIdx2)) match {
                      case Some(child2) => matchesTermNodes(child2, term2, child1, term1)
                      case None         => false
                    }
                  case (false, _)      =>
                    false
                }
              case None           =>
                false
            }
          case (false, _)     =>
            false
        }
      case _ =>
        false
    }
  }
  
  override def matches(term1: MatchingTerm, term2: MatchingTerm, matching: Matching.Value) = {
    // term1.root is superconjunction node
    // term2.root is conjunction node
    matchesTermNodes(term1.root, term1, term2.root, term2).success
  }
}