/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.logicalterm
import scalaz._
import scalaz.Scalaz._

sealed trait Term
{
  def & (term: Term) =
    (this, term) match {
      case (Conjunction(terms), Conjunction(terms2)) => Conjunction(terms | terms2)
      case (Conjunction(terms), _)                   => Conjunction(terms + term)
      case (_, Conjunction(terms))                   => Conjunction(terms + this)
      case (_, _)                                    => Conjunction(Set(this, term))
    }

  def | (term: Term) =
    (this, term) match {
      case (Disjunction(terms), Disjunction(terms2)) => Disjunction(terms | terms2)
      case (Disjunction(terms), _)                   => Disjunction(terms + term)
      case (_, Disjunction(terms))                   => Disjunction(terms + this)
      case (_, _)                                    => Disjunction(Set(this, term))
    }
  
  private def flattenConjunction: Term =
    this match {
      case Conjunction(terms) =>
        terms.headOption.map { 
          _ => terms.foldLeft(Conjunction(Set())) { _.flattenConjunction & _.flattenConjunction }
        }.getOrElse(Conjunction(Set()))
      case _                  =>
        this
    }

  private def flattenDisjunction: Term =
    this match {
      case Disjunction(terms) =>
        terms.headOption.map { 
          _ => terms.foldLeft(Disjunction(Set())) { _.flattenDisjunction | _.flattenDisjunction }
        }.getOrElse(Disjunction(Set()))
      case _                  =>
        this
    }
  
  def normalizedTerm =
    this match {
      case Conjunction(terms) =>
        val conj = flattenConjunction
        terms.headOption.map { if(terms.size === 1) _ else conj }.getOrElse(conj)
      case Disjunction(terms) =>
        val disj = flattenDisjunction
        terms.headOption.map { if(terms.size === 1) _ else disj }.getOrElse(disj)
      case _                  =>
        this
    }
  
  def distributedTerm =
    this match {
      case Conjunction(terms) =>
        val (disjs, otherTerms) = terms.partition {
          case Disjunction(ts) => ts.size > 1
          case _               => false
        }
        disjs.headOption.flatMap {
          case Disjunction(terms2) =>
            terms2.headOption.flatMap {
              term2 =>
                val conj2 = Conjunction(disjs.tail | otherTerms).normalizedTerm
                terms2.tail.headOption.map {
                  _ =>
                    val disj2 = Disjunction(terms2.tail).normalizedTerm
                    (term2 & conj2) | (disj2 & conj2)
                }
            }
          case _ =>
            none
        }
      case Disjunction(terms) =>
        val (conjs, otherTerms) = terms.partition {
          case Conjunction(ts) => ts.size > 1
          case _               => false
        }
        conjs.headOption.flatMap {
          case Conjunction(terms2) =>
            terms2.headOption.flatMap {
              term2 =>
                val disj2 = Disjunction(conjs.tail | otherTerms).normalizedTerm
                terms2.tail.headOption.map {
                  _ =>
                    val conj2 = Conjunction(terms2.tail).normalizedTerm
                    (term2 | disj2) & (conj2 | disj2)
                }
            }
          case _ =>
            none
        }
      case _ =>
        none
    }
  
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
      case Conjunction(terms) =>
        terms.map {
          case term @ (Disjunction(_) | Conjunction(_)) => term.toArgString
          case term                                     => term.toString
        }.mkString(" & ")
      case Disjunction(terms) =>
        terms.map {
          case term @ Disjunction(_) => term.toArgString
          case term                  => term.toString
        }.mkString(" | ")
    }
}

case class VarApp(name: String, args: Seq[Term]) extends Term
sealed trait LogicalTerm extends Term
{
  def terms: Set[Term]
}
case class Conjunction(terms: Set[Term]) extends LogicalTerm
case class Disjunction(terms: Set[Term]) extends LogicalTerm
