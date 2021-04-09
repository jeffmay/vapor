package com.rallyhealth

package vapors

package object data {

  /**
    * Special case of [[ExtractValue]] that extracts a [[Boolean]] value.
    */
  final type ExtractBoolean[-T] = ExtractValue[T, Boolean]

  object ExtractBoolean {

    @deprecated("Use ExtractValue[Boolean].from[T]", "0.14.1")
    @inline final def apply[T](implicit instance: ExtractBoolean[T]): ExtractBoolean[T] = instance
  }

  /**
    * An ordered set of untyped [[Fact]]s.
    */
  final type FactSet = Set[Fact]

  /**
    * An ordered set of [[TypedFact]]s.
    *
    * @note not to be confused with a [[FactTypeSet]] (which is a set of [[FactType]]s, with not values)
    */
  final type TypedFactSet[T] = Set[TypedFact[T]]
}
