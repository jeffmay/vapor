package com.rallyhealth.vapors.factfilter.data

import cats.{Foldable, Order}
import cats.instances.order._

import scala.collection.immutable.SortedSet

object FactSet {

  final val empty: FactSet = SortedSet.empty[Fact]

  @inline final def apply(facts: Fact*)(implicit order: Order[Fact]): FactSet = SortedSet.from(facts)

  @inline final def from(facts: Iterable[Fact])(implicit order: Order[Fact]): FactSet = SortedSet.from(facts)

  @inline final def fromFoldable[F[_] : Foldable](facts: F[Fact])(implicit order: Order[Fact]): FactSet = {
    SortedSet.from(Foldable[F].toIterable(facts))
  }
}

object TypedFactSet {

  @inline final def empty[T : OrderTypedFacts]: TypedFactSet[T] = SortedSet.empty[TypedFact[T]]

  @inline final def apply[T : OrderTypedFacts](facts: TypedFact[T]*): TypedFactSet[T] = SortedSet.from(facts)

  @inline final def from[T : OrderTypedFacts](facts: Iterable[TypedFact[T]]): TypedFactSet[T] = SortedSet.from(facts)
}
