package com.rallyhealth.vapors.factfilter.data

import com.rallyhealth.vapors.core.data.{DataPath, NamedLens}

sealed abstract class Fact {
  type Value

  val typeInfo: FactType[Value]
  val value: Value

  override def toString: String = {
    // TODO: JSON format for value? Show for value?
    s"Fact(${typeInfo.fullName} = $value)"
  }
}

object Fact {

  @inline final def apply[T](
    factType: FactType[T],
    value: T,
  ): Fact = TypedFact(factType, value)
}

final case class TypedFact[A](
  typeInfo: FactType[A],
  value: A,
) extends Fact {
  type Value = A
}

object TypedFact {

  final def lens[A]: NamedLens.Id[TypedFact[A]] = NamedLens.id[TypedFact[A]]

  final def value[A]: NamedLens[TypedFact[A], A] = {
    NamedLens[TypedFact[A], A](DataPath.empty.atField("value"), _.value)
  }
}
