package com.rallyhealth.vapors.core.data

import cats.instances.order._
import cats.{Eq, Monoid}
import com.rallyhealth.vapors.core.lens.Indexed

import scala.collection.immutable.SortedMap

/**
  * The current state of all the facts in an expression.
  *
  * @note some expressions can update the fact table for sub-expressions.
  */
final case class FactTable(factsByName: SortedMap[String, FactSet]) extends AnyVal {

  def add(fact: Fact): FactTable = addAll(FactSet(fact))

  def addAll(facts: Iterable[Fact]): FactTable = {
    import cats.syntax.semigroup._
    val newFactTable = FactTable(facts)
    new FactTable(this.factsByName.combine(newFactTable.factsByName))
  }

  def getSortedSeq[T](factTypeSet: FactTypeSet[T]): Seq[TypedFact[T]] = {
    val sortedArray = getSet(factTypeSet).toArray.sortInPlace()
    sortedArray.to(LazyList)
  }

  def getSet[T](factTypeSet: FactTypeSet[T]): TypedFactSet[T] = {
    val matchingFacts = for {
      factName <- factTypeSet.typeMap.toSortedMap.keys
      matchingFactsByName <- this.factsByName.get(factName)
    } yield matchingFactsByName.collect {
      case factTypeSet(matchingByType) => matchingByType
    }
    matchingFacts.reduceOption(_ | _).map(TypedFactSet.from).getOrElse(Set.empty)
  }
}

object FactTable {

  final val empty = new FactTable(SortedMap.empty)

  def apply(facts: FactOrFactSet*): FactTable = {
    val factSet = FactOrFactSet.flatten(facts)
    if (factSet.isEmpty) empty
    else new FactTable(SortedMap.from(factSet.groupBy(_.typeInfo.fullName)))
  }

  implicit object MonoidInstance extends Monoid[FactTable] {
    override def empty: FactTable = FactTable.empty
    override def combine(
      x: FactTable,
      y: FactTable,
    ): FactTable = {
      import cats.syntax.semigroup._
      new FactTable(x.factsByName |+| y.factsByName)
    }
  }

  implicit val eq: Eq[FactTable] = Eq.fromUniversalEquals

  implicit def indexedByFactType[T]: Indexed[FactTable, FactType[T], TypedFactSet[T]] = {
    new Indexed[FactTable, FactType[T], TypedFactSet[T]] {
      override def get(container: FactTable)(key: FactType[T]): TypedFactSet[T] = {
        container.getSet(key)
      }
    }
  }

  implicit def indexedByFactTypeSet[T]: Indexed[FactTable, FactTypeSet[T], TypedFactSet[T]] = {
    new Indexed[FactTable, FactTypeSet[T], TypedFactSet[T]] {
      override def get(container: FactTable)(key: FactTypeSet[T]): TypedFactSet[T] = container.getSet(key)
    }
  }
}
