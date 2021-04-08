package com.rallyhealth.vapors

package core.data

import cats.Foldable

/**
  * @see [[FactSet]]
  */
object FactSet {

  final val empty: FactSet = Set.empty[Fact]

  @inline final def apply(facts: Fact*): FactSet = Set.from(facts)

  @inline final def from(facts: IterableOnce[Fact]): FactSet = Set.from(facts)

  @inline final def fromFoldable[F[_] : Foldable](facts: F[Fact]): FactSet = {
    Set.from(Foldable[F].toIterable(facts))
  }
}

/**
  * @see [[TypedFactSet]]
  */
object TypedFactSet {

  @inline final def empty[T]: TypedFactSet[T] = Set.empty[TypedFact[T]]

  @inline final def apply[T](facts: TypedFact[T]*): TypedFactSet[T] = Set.from(facts)

  @inline final def from[T](facts: Iterable[TypedFact[T]]): TypedFactSet[T] = Set.from(facts)
}
