package pl.luckboy.logicalterm.hash
import scala.collection.immutable.IntMap
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.logicalterm
import pl.luckboy.logicalterm.Term
import pl.luckboy.logicalterm.Matcher
import pl.luckboy.logicalterm.Matching
import Utils._

class MatchingTermMatcher extends Matcher[MatchingTerm]
{
  private def recursivelyMatchingTermFromTerm(term: Term): MatchingTerm =
    term.normalizedTerm match {
      case logicalterm.VarApp(name, args) =>
        VarApp(name, args.map(recursivelyMatchingTermFromTerm).toVector)
      case logicalterm.Conjunction(terms) =>
        Conjunction(terms.foldLeft(IntMap[List[MatchingTerm]]()) {
          (terms2, term) =>
            val term2 = recursivelyMatchingTermFromTerm(term)
            terms2 + (term2.lazyHashCode -> (term2 :: terms2.getOrElse(term2.lazyHashCode, Nil)))
        })
      case logicalterm.Disjunction(terms) =>
        Disjunction(terms.foldLeft(IntMap[List[MatchingTerm]]()) {
          (terms2, term) =>
            val term2 = recursivelyMatchingTermFromTerm(term)
            terms2 + (term2.lazyHashCode -> (term2 :: terms2.getOrElse(term2.lazyHashCode, Nil)))
        })
    }
  
  override def matchingTermFromTerm(term: Term) = some(recursivelyMatchingTermFromTerm(term))
    
  private def matchesForOneTerm(term: MatchingTerm, terms: Iterable[MatchingTerm], matching: Matching.Value) =
    terms.foldLeft(false) {
      (b, term2) => if(b) true else matchesTerms(term, term2, matching)
    }
 
  private def matchesForAllTerms(term: MatchingTerm, terms: IntMap[List[MatchingTerm]], matching: Matching.Value) =
    terms.foldLeft(true) {
      case (b, (_, terms2)) => if(b) terms2.foldLeft(b) { (b2, term2) => if(b2) matchesTerms(term, term2, matching) else false } else false
    }
      
  private def matchesSupertermWithTerm(term1: MatchingTerm, term2: MatchingTerm): Boolean =
    (term1, term2) match {
      case (Disjunction(terms1), Conjunction(terms2)) =>
        term1.distributedTerm.map { (_, term2) }.orElse {
          term2.distributedTerm.map { (term1, _) }
        }.map {
          case (distributedTerm1, distributedTerm2) => matchesSupertermWithTerm(distributedTerm1, distributedTerm2)
        }.getOrElse {
          matchesForOneTerm(term1, filterTermsFromSuperterm(terms2, term1), Matching.SupertermWithTerm) ||
          matchesForOneTerm(term2, filterSupertermsFromTerm(terms1, term2), Matching.TermWithSuperterm)
        }
      case (_, Disjunction(terms2)) =>
        matchesForAllTerms(term1, terms2, Matching.SupertermWithTerm)
      case (Conjunction(terms1), _) =>
        matchesForAllTerms(term2, terms1, Matching.TermWithSuperterm)
      case (_, Conjunction(terms2)) =>
        matchesForOneTerm(term1, filterTermsFromSuperterm(terms2, term1), Matching.SupertermWithTerm)
      case (Disjunction(terms1), _) =>
        matchesForOneTerm(term2, filterSupertermsFromTerm(terms1, term2), Matching.TermWithSuperterm)
      case (varApp1: VarApp, varApp2: VarApp) =>
        matchesVarApps(varApp1, varApp2)
    }
  
  private def matchesVarApps(varApp1: VarApp, varApp2: VarApp) =
    (varApp1, varApp2) match {
      case (VarApp(name1, args1), VarApp(name2, args2)) if name1 === name2 && args1.size === args2.size =>
        args1.zip(args2).foldLeft(true) {
          case (b, (arg1, arg2)) => if(b) matchesTerms(arg1, arg2, Matching.Terms) else false
        }
      case _ =>
        false
    }
  
  private def matchesLogicalTerms(term1: MatchingTerm, term2: MatchingTerm, matching: Matching.Value) =
    matching match {
      case Matching.Terms             =>
        matchesHashCodes(term1.lazyHashCode, term2.lazyHashCode) &&
        matchesSupertermWithTerm(term1, term2) && matchesSupertermWithTerm(term2, term1)
      case Matching.SupertermWithTerm =>
        matchesSuperHashCodeWithHashCode(term1.lazyHashCode, term2.lazyHashCode) &&
        matchesSupertermWithTerm(term1, term2)
      case Matching.TermWithSuperterm =>
        matchesSuperHashCodeWithHashCode(term2.lazyHashCode, term1.lazyHashCode) &&
        matchesSupertermWithTerm(term2, term1)
    }
  
  private def matchesTerms(term1: MatchingTerm, term2: MatchingTerm, matching: Matching.Value): Boolean =
    (term1, term2) match {
      case (varApp1: VarApp, varApp2: VarApp)   => matchesVarApps(varApp1, varApp2)
      case (Conjunction(_) | Disjunction(_), _) => matchesLogicalTerms(term1, term2, matching)
      case (_, Conjunction(_) | Disjunction(_)) => matchesLogicalTerms(term1, term2, matching)
    }

  override def matches(term1: MatchingTerm, term2: MatchingTerm, matching: Matching.Value) =
    matchesTerms(term1, term2, matching).success
}