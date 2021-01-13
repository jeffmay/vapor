package com.rallyhealth.vapors.factfilter.data

import cats.Order
import cats.syntax.all._
import com.rallyhealth.vapors.core.data.{DataPath, NamedLens}

sealed abstract class Fact {
  type Value

  val typeInfo: FactType[Value]
  val value: Value
}

object Fact {

  def apply[T](
    factType: FactType[T],
    value: T,
  ): Fact = SourceFactOfType(factType, value)

  def apply[T](
    factType: FactType[T],
    value: T,
    evidence: Evidence,
  ): DerivedFact = DerivedFactOfType(factType, value, evidence)

  def unapply(fact: Fact): Some[(FactType[fact.Value], fact.Value)] = Some((fact.typeInfo, fact.value))

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

sealed trait TypedFact[A] extends Fact {
  type Value = A
}

object TypedFact {

  def apply[A](
    typeInfo: FactType[A],
    value: A,
  ): TypedFact[A] = SourceFactOfType(typeInfo, value)

  def apply[A](
    typeInfo: FactType[A],
    value: A,
    evidence: Evidence,
  ): TypedFact[A] with DerivedFact = DerivedFactOfType(typeInfo, value, evidence)

  def unapply[A](fact: TypedFact[A]): Some[(FactType[A], A)] = fact match {
    case SourceFactOfType(typeInfo, value) => Some((typeInfo, value))
    case DerivedFactOfType(typeInfo, value, _) => Some((typeInfo, value))
  }

  def orderByTypedFactValue[T]: Order[TypedFact[T]] = { (x, y) =>
    x.typeInfo.order.compare(x.value, y.value)
  }

  implicit def order[T](implicit orderFactNames: Order[String]): Order[TypedFact[T]] = { (x, y) =>
    orderFactNames.compare(x.typeInfo.name, y.typeInfo.name) match {
      case 0 => orderByTypedFactValue[T].compare(x, y)
      case orderByName => orderByName
    }
  }

  def lens[A]: NamedLens.Id[TypedFact[A]] = NamedLens.id[TypedFact[A]]

  def value[A]: NamedLens[TypedFact[A], A] = {
    NamedLens[TypedFact[A], A](DataPath.empty.atField("value"), _.value)
  }
}

sealed trait DerivedFact extends Fact {
  def evidence: Evidence
}

object DerivedFact {

  def apply[A](
    typeInfo: FactType[A],
    value: A,
    evidence: Evidence,
  ): DerivedFact =
    DerivedFactOfType(typeInfo, value, evidence)

  def unapply(fact: Fact): Option[(FactType[fact.Value], fact.Value, Evidence)] = fact match {
    case DerivedFactOfType(_, _, evidence) =>
      Some((fact.typeInfo, fact.value, evidence))
    case _ => None
  }
}

final case class SourceFactOfType[A](
  typeInfo: FactType[A],
  value: A,
) extends TypedFact[A] {

  override def toString: String = s"SourceFact(${typeInfo.fullName} = $value)"
}

final case class DerivedFactOfType[A](
  typeInfo: FactType[A],
  value: A,
  evidence: Evidence,
) extends TypedFact[A]
  with DerivedFact {

  override def toString: String = s"DerivedFact(${typeInfo.fullName} = $value, evidence = $evidence)"
}
