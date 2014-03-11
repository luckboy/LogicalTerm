package pl.luckboy.logicalterm.simple
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.logicalterm._

class TermMatcher extends Matcher[Term]
{
  override def matchingTermFromTerm(term: Term) = some(term.normalizedTerm)
  
  private def matchesForOneTerm(term: Term, terms: Set[Term], matching: Matching.Value) =
    terms.foldLeft(false) {
      (b, term2) => if(b) true else matchesTerms(term, term2, matching)
    }
 
  private def matchesForAllTerms(term: Term, terms: Set[Term], matching: Matching.Value) =
    terms.foldLeft(true) {
      (b, term2) => if(b) matchesTerms(term, term2, matching) else false
    }
  
  private def matchesSupertermWithTerm(term1: Term, term2: Term): Boolean =
    (term1, term2) match {
      case (Disjunction(terms1), Conjunction(terms2)) =>
        term1.distributedTerm.map { (_, term2) }.orElse {
          term2.distributedTerm.map { (term1, _) }
        }.map {
          case (distributedTerm1, distributedTerm2) => matchesSupertermWithTerm(distributedTerm1, distributedTerm2)
        }.getOrElse {
          matchesForOneTerm(term1, terms2, Matching.SupertermWithTerm) ||
          matchesForOneTerm(term2, terms1, Matching.TermWithSuperterm)
        }
      case (_, Disjunction(terms2)) =>
        matchesForAllTerms(term1, terms2, Matching.SupertermWithTerm)
      case (Conjunction(terms1), _) =>
        matchesForAllTerms(term2, terms1, Matching.TermWithSuperterm)
      case (_, Conjunction(terms2)) =>
        matchesForOneTerm(term1, terms2, Matching.SupertermWithTerm)
      case (Disjunction(terms1), _) =>
        matchesForOneTerm(term2, terms1, Matching.TermWithSuperterm)
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
  
  private def matchesLogicalTerms(term1: Term, term2: Term, matching: Matching.Value) =
    matching match {
      case Matching.Terms             =>
        matchesSupertermWithTerm(term1, term2) && matchesSupertermWithTerm(term2, term1)
      case Matching.SupertermWithTerm =>
        matchesSupertermWithTerm(term1, term2)
      case Matching.TermWithSuperterm =>
        matchesSupertermWithTerm(term2, term1)
    }
  
  private def matchesTerms(term1: Term, term2: Term, matching: Matching.Value): Boolean =
    (term1, term2) match {
      case (varApp1: VarApp, varApp2: VarApp)   => matchesVarApps(varApp1, varApp2)
      case (Conjunction(_) | Disjunction(_), _) => matchesLogicalTerms(term1, term2, matching)
      case (_, Conjunction(_) | Disjunction(_)) => matchesLogicalTerms(term1, term2, matching)
    }
    
  override def matches(term1: Term, term2: Term, matching: Matching.Value) =
    matchesTerms(term1, term2, matching).success
}