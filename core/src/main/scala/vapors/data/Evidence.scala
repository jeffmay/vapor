package com.rallyhealth

package vapors.data

import cats.Monoid
import cats.data.NonEmptySet
import cats.instances.order._

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

/**
  * A set of [[Fact]]s used to derive an evaluated expression result.
  *
  * If the resulting evidence is empty, then you should treat the value as identical to its negation.
  *
  * TODO: there are some bugs with evidence tracking of collection-level operations and how constants
  *       are handled, so you probably shouldn't rely on this right now.
  */
final class Evidence private (val factSet: Set[Fact]) extends AnyVal {

  def isEmpty: Boolean = factSet.isEmpty
  def nonEmpty: Boolean = factSet.nonEmpty

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

  @inline def &(that: Evidence): Evidence = combineNonEmpty(that)

  /**
    * If either side contains empty evidence then the result contains no evidence.
    */
  def combineNonEmpty(that: Evidence): Evidence = {
    if (this.isEmpty || that.isEmpty) Evidence.none
    else this.union(that)
  }

  def derivedFromSources: Evidence = {
    @tailrec def loop(
      mixed: Iterable[Fact],
      source: FactSet,
    ): FactSet = {
      if (mixed.isEmpty) source
      else {
        val (remainingEvidence, sourceFacts) = mixed.partitionMap {
          case DerivedFact(_, _, evidence) => Left(evidence.factSet)
          case source => Right(source)
        }
        loop(remainingEvidence.flatten, source ++ sourceFacts)
      }
    }
    val allSourceFacts = loop(this.factSet, FactSet.empty)
    new Evidence(allSourceFacts)
  }

  override def toString: String =
    if (this.factSet.isEmpty) "Evidence()"
    else s"Evidence${factSet.mkString("(", ", ", ")")}"
}

object Evidence {

  def unapply(evidence: Evidence): Some[Set[Fact]] = Some(evidence.factSet)

  @inline final def apply(facts: FactOrFactSet*): Evidence = {
    if (facts.isEmpty) none
    else new Evidence(FactOrFactSet.flatten(facts))
  }

  final val none = new Evidence(Set.empty)

  /**
    * Convert any given value into [[Evidence]] by inspecting whether it is a fact or valid collection of facts.
    *
    * This is used by the library when iterating over a collection of facts, where the facts can be used as their own
    * evidence in the subexpression.
    */
  def fromAny(any: Any): Option[Evidence] = any match {
    case ev: Evidence => Some(ev)
    case fact: Fact => Some(Evidence(fact))
    case map: collection.Map[_, _] => fromAnyIterable(map.valuesIterator)
    case iter: IterableOnce[_] => fromAnyIterable(iter)
    case _ => None
  }

  @inline def fromAnyOrNone(any: Any): Evidence = fromAny(any).getOrElse(none)

  private[this] def fromAnyIterable(anyIter: IterableOnce[_]): Option[Evidence] = {
    val iter = anyIter.iterator
    if (iter.isEmpty) None
    else Some(Evidence(FactSet.from(iter.collect { case fact: Fact => fact })))
  }

  /**
    * Unions evidence as a standard definition for monoid.
    *
    * This is the right behavior to satify [[Monoid]], however if you need to do evidence tracking,
    * you wlil probably want to be more specific if you need to represent the behavior of merging
    * evidence from the output of various expressions (or their input).
    *
    * @see [[InterpretExprAsResultFn.Output.monoid]]
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
