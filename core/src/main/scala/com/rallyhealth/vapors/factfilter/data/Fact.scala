package com.rallyhealth.vapors.factfilter.data

import cats.Order
import cats.syntax.all._
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

  val orderByFactValue: Order[Fact] = { (x, y) =>
    def maybeXCompared =
      x.typeInfo
        .cast(y)
        .map(yAsX => x.typeInfo.order.compare(x.value, yAsX.value))

    def maybeYCompared =
      y.typeInfo
        .cast(x)
        .map(xAsY => y.typeInfo.order.compare(xAsY.value, y.value))

    def fallbackToCompareAsStrings =
      if (x.value == y.value) 0
      else Order[String].compare(x.value.toString, y.value.toString)

    maybeXCompared.orElse(maybeYCompared).getOrElse(fallbackToCompareAsStrings)
  }

  def orderByFactName(nameOrder: Order[String]): Order[Fact] = {
    nameOrder.contramap(_.typeInfo.name)
  }

  // This will be used a lot, so cache it
  private final val defaultOrder: Order[Fact] = orderByFactName(Order[String])

  implicit def order(implicit orderFactNames: Order[String]): Order[Fact] = {
    if (orderFactNames == cats.instances.string.catsKernelStdOrderForString) defaultOrder
    else Order.whenEqual(orderByFactName(orderFactNames), orderByFactValue)
  }
}

final case class TypedFact[A](
  typeInfo: FactType[A],
  value: A,
) extends Fact {
  type Value = A
}

object TypedFact {

  def orderByTypedFactValue[T]: Order[TypedFact[T]] = { (x, y) =>
    x.typeInfo.order.compare(x.value, y.value)
  }

  implicit def order[T](implicit orderFactNames: Order[String]): Order[TypedFact[T]] = { (x, y) =>
    orderFactNames.compare(x.typeInfo.name, y.typeInfo.name) match {
      case 0 => orderByTypedFactValue[T].compare(x, y)
      case orderByName => orderByName
    }
  }

  final def lens[A]: NamedLens.Id[TypedFact[A]] = NamedLens.id[TypedFact[A]]

  final def value[A]: NamedLens[TypedFact[A], A] = {
    NamedLens[TypedFact[A], A](DataPath.empty.atField("value"), _.value)
  }
}
