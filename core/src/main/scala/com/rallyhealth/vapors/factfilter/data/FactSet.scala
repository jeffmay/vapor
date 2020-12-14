package com.rallyhealth.vapors.factfilter.data

import cats.Foldable

import scala.collection.immutable.SortedSet

object FactSet {

  final val empty: FactSet = SortedSet.empty[Fact]

  @inline final def apply(facts: Fact*): FactSet = SortedSet.from(facts)

  @inline final def from(facts: Iterable[Fact]): FactSet = SortedSet.from(facts)

  @inline final def fromFoldable[F[_] : Foldable](facts: F[Fact]): FactSet = {
    SortedSet.from(Foldable[F].toIterable(facts))
  }
}

object TypedFactSet {

  @inline final def empty[T]: TypedFactSet[T] = SortedSet.empty[TypedFact[T]]

  @inline final def apply[T](facts: TypedFact[T]*): TypedFactSet[T] = SortedSet.from(facts)

  @inline final def from[T](facts: Iterable[TypedFact[T]]): TypedFactSet[T] = SortedSet.from(facts)
}
