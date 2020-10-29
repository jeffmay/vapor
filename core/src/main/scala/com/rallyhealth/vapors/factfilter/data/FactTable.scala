package com.rallyhealth.vapors.factfilter.data

import com.rallyhealth.vapors.core.data.Indexed

/**
  * The current state of all the facts in an expression.
  *
  * @note some expressions can update the fact table for sub-expressions.
  */
// TODO: Use a SortedSet?
final case class FactTable private (private val facts: Map[String, List[Fact]]) {
  import cats.syntax.semigroup._
  import cats.instances.map._
  import cats.instances.list._

  def add(fact: Fact): FactTable = addAll(fact :: Nil)

  def addAll(facts: List[Fact]): FactTable = {
    val newFacts = facts.groupBy(_.typeInfo.fullName)
    new FactTable(this.facts |+| newFacts)
  }

  def get[T](factType: FactType[T]): List[TypedFact[T]] =
    facts.get(factType.fullName).toList.flatMap(_.collect(Function.unlift(factType.cast)))

  def getAll[T](factTypeSet: FactTypeSet[T]): List[TypedFact[T]] = {
    factTypeSet.typeMap.toSortedMap.values.foldLeft(List.empty[TypedFact[T]]) {
      case (acc, ft) =>
        acc ::: get(ft)
    }
  }
}

object FactTable {

  final val empty = FactTable(Nil)

  def apply(facts: List[Fact]): FactTable = {
    if (facts eq Nil) empty
    else new FactTable(facts.groupBy(_.typeInfo.fullName))
  }

  implicit def indexedByFactType[T]: Indexed[FactTable, FactType[T], List[TypedFact[T]]] = {
    new Indexed[FactTable, FactType[T], List[TypedFact[T]]] {
      override def get(container: FactTable)(key: FactType[T]): List[TypedFact[T]] = {
        container.get(key)
      }
    }
  }

  implicit def indexedByFactTypeSet[T]: Indexed[FactTable, FactTypeSet[T], List[TypedFact[T]]] = {
    new Indexed[FactTable, FactTypeSet[T], List[TypedFact[T]]] {
      override def get(container: FactTable)(key: FactTypeSet[T]): List[TypedFact[T]] = container.getAll(key)
    }
  }
}
