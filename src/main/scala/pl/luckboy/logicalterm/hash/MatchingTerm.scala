package pl.luckboy.logicalterm.hash
import scala.collection.immutable.IntMap
import scalaz._
import scalaz.Scalaz._

sealed trait MatchingTerm
{
  def distributedTerm =
    this match {
      case conj: Conjunction =>
        val (disjs, otherTerms) = conj.termIterable.partition {
          case Disjunction(ts) => ts.size > 1 || ts.values.headOption.map { _.size > 1 }.getOrElse(false)
          case _               => false
        }
        disjs.headOption.flatMap {
          case disj: Disjunction =>
            val terms2 = disj.termIterable
            terms2.headOption.flatMap {
              term2 =>
                val conj2 = Conjunction.fromIterable(disjs.tail.toSet | otherTerms.toSet).normalizedTerm
                terms2.tail.headOption.map {
                  _ =>
                    val disj2 = Disjunction.fromIterable(terms2.tail).normalizedTerm
                    Disjunction.fromIterable(List(
                        Conjunction.fromIterable(List(term2, conj2)), 
                        Conjunction.fromIterable(List(disj2, conj2))))
                }
            }
          case _ =>
            none
        }
      case disj: Disjunction =>
        val (conjs, otherTerms) = disj.termIterable.partition {
          case Conjunction(ts) => ts.size > 1 || ts.values.headOption.map { _.size > 1 }.getOrElse(false)
          case _               => false
        }
        conjs.headOption.flatMap {
          case conj: Conjunction =>
            val terms2 = conj.termIterable
            terms2.headOption.flatMap {
              term2 =>
                val disj2 = Disjunction.fromIterable(conjs.tail.toSet | otherTerms.toSet).normalizedTerm
                terms2.tail.headOption.map {
                  _ =>
                    val conj2 = Conjunction.fromIterable(terms2.tail).normalizedTerm
                    Conjunction.fromIterable(List(
                        Disjunction.fromIterable(List(term2, disj2)),
                        Disjunction.fromIterable(List(conj2, disj2))))
                }
            }
          case _ =>
            none
        }
      case _ =>
        none
    }
  
  def normalizedTerm =
    this match {
      case conj: Conjunction =>
        val terms2 = conj.termIterable.foldLeft(Set[MatchingTerm]()) {
          case (ts, conj2: Conjunction) => ts ++ conj2.termIterable
          case (ts, term)               => ts + term
        }
        terms2.headOption.map { if(terms2.size === 1) _ else Conjunction.fromIterable(terms2) }.getOrElse(Conjunction.fromIterable(terms2))
      case disj: Disjunction =>
        val terms2 = disj.termIterable.foldLeft(Set[MatchingTerm]()) {
          case (ts, disj2: Disjunction) => ts ++ disj2.termIterable
          case (ts, term)               => ts + term
        }
        terms2.headOption.map { if(terms2.size === 1) _ else Disjunction.fromIterable(terms2) }.getOrElse(Disjunction.fromIterable(terms2))
      case _                 =>
        this
    }
  
  def lazyHashCode: Int
  
  private def toArgString: String =
    this match {
      case VarApp(_, args) if !args.isEmpty   => "(" + this + ")"
      case Conjunction(_)                     => "(" + this + ")"
      case Disjunction(_)                     => "(" + this + ")"
      case _                                  => toString
    }
  
  override def toString =
    this match {
      case VarApp(name, args) => name + args.map { " " + _.toArgString }.mkString
      case conj: Conjunction  =>
        conj.termIterable.map {
          case term @ (Disjunction(_) | Conjunction(_)) => term.toArgString
          case term                                     => term.toString
        }.mkString(" & ")
      case disj: Disjunction  =>
        disj.termIterable.map {
          case term @ Disjunction(_) => term.toArgString
          case term                  => term.toString
        }.mkString(" | ")
    }
}

case class VarApp(name: String, args: Vector[MatchingTerm]) extends MatchingTerm
{
  override lazy val lazyHashCode = hashCode
}

case class Conjunction(terms: IntMap[List[MatchingTerm]]) extends MatchingTerm
{
  def termIterable = terms.flatMap { _._2 }
  
  override def hashCode = lazyHashCode
  
  override lazy val lazyHashCode =
    terms.keys.headOption.map { k => terms.keys.tail.foldLeft(k) { _ & _ } }.getOrElse(0)
}

object Conjunction
{
  def fromIterable(terms: Iterable[MatchingTerm]) =
    Conjunction(terms.foldLeft(IntMap[List[MatchingTerm]]()) {
      (ts, t) => ts + (t.lazyHashCode, t :: ts.getOrElse(t.lazyHashCode, Nil))
    })
}

case class Disjunction(terms: IntMap[List[MatchingTerm]]) extends MatchingTerm
{
  def termIterable = terms.flatMap { _._2 }
  
  override def hashCode = lazyHashCode
  
  override lazy val lazyHashCode = 
    terms.keys.headOption.map { k => terms.keys.tail.foldLeft(k) { _ | _ } }.getOrElse(0)
}

object Disjunction
{
  def fromIterable(terms: Iterable[MatchingTerm]) =
    Disjunction(terms.foldLeft(IntMap[List[MatchingTerm]]()) {
      (ts, t) => ts + (t.lazyHashCode, t :: ts.getOrElse(t.lazyHashCode, Nil))
    })
}