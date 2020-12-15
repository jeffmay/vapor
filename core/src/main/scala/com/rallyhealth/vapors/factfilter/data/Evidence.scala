package com.rallyhealth.vapors.factfilter.data

import cats.Monoid
import cats.data.NonEmptySet

import scala.collection.immutable.SortedSet

final class Evidence private (val factSet: SortedSet[Fact]) extends AnyVal {

  def isEmpty: Boolean = factSet.isEmpty
  def nonEmpty: Boolean = factSet.nonEmpty

  @deprecated("Use factSet instead.", "0.3.0")
  def facts: List[Fact] = factSet.toList

  @inline def ++(that: Evidence): Evidence = union(that)
  @inline def |(that: Evidence): Evidence = union(that)

  def ofType[T](factTypeSet: FactTypeSet[T]): Option[NonEmptySet[TypedFact[T]]] = {
    val matchingFacts = SortedSet.from(this.factSet.iterator.collect(factTypeSet.collector))
    NonEmptySet.fromSet(matchingFacts)
  }

  /**
    * Union of the two sets of results.
    */
  def union(that: Evidence): Evidence = this.factSet match {
    case Evidence.none.factSet | that.factSet => that
    case _ => Evidence(this.factSet | that.factSet)
  }

  override def toString: String =
    if (this.factSet.isEmpty) "Evidence()"
    else s"Evidence${factSet.mkString("(", ", ", ")")}"
}

object Evidence {

  def unapply(evidence: Evidence): Some[SortedSet[Fact]] = Some(evidence.factSet)

  final class SetOfFacts private[Evidence] (private[Evidence] val factSet: SortedSet[Fact]) extends AnyVal

  implicit def singleSetOfFacts(fact: Fact): SetOfFacts = new SetOfFacts(SortedSet(fact))
  implicit def iterableSetOfFacts(facts: Iterable[Fact]): SetOfFacts = new SetOfFacts(SortedSet.from(facts))

  @inline final def apply(factSets: SetOfFacts*): Evidence = {
    if (factSets.isEmpty) none
    else new Evidence(factSets.iterator.foldLeft(none.factSet)(_ | _.factSet))
  }

  final val none = new Evidence(SortedSet.empty[Fact])

  /**
    * Unions evidence as a standard definition for monoid.
    *
    * This is the right behavior to satify [[Monoid]], however if you need to do evidence tracking,
    * you wlil probably want to be more specific if you need to represent the behavior of merging
    * evidence from the output of various expressions (or their input).
    *
    * @see [[com.rallyhealth.vapors.factfilter.evaluator.InterpretExprAsFunction.Output.monoid]]
    */
  implicit val monoid: Monoid[Evidence] = {
    new Monoid[Evidence] {
      override def empty: Evidence = Evidence.none
      override def combine(
        x: Evidence,
        y: Evidence,
      ): Evidence = x.union(y)
    }
  }
}
