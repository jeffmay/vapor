package com.rallyhealth.vapors.factfilter.data

import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet}

import scala.collection.immutable

final case class FactTypeSet[A] private (typeMap: NonEmptyMap[String, FactType[A]]) {

  lazy val typeList: NonEmptyList[FactType[A]] = typeMap.toNel.map(_._2)

  lazy val typeSet: NonEmptySet[FactType[A]] = typeList.toNes

  private val castPF: PartialFunction[Fact, TypedFact[A]] = {
    case fact @ TypedFact(factType, _) if typeMap(factType.fullName).contains(factType) =>
      // Justification: This checks equality of the FactType at runtime after safely casting the value
      fact.asInstanceOf[TypedFact[A]]
  }

  private val castF: Fact => Option[TypedFact[A]] = castPF.lift

  /**
    * Checks the [[FactType]] for equality with one of the types in this set.
    *
    * @return the [[TypedFact]] as the expected type.
    */
  def cast(fact: Fact): Option[TypedFact[A]] = castF(fact)

  /**
    *
    * @return a partial function for collecting facts of a specific type
    */
  def collector: PartialFunction[Fact, TypedFact[A]] = castPF

}

object FactTypeSet {

  implicit def one[A](factType: FactType[A]): FactTypeSet[A] = of(factType)

  def of[A](
    one: FactType[A],
    others: FactType[A]*,
  ): FactTypeSet[A] = {
    import cats.instances.string._
    new FactTypeSet(NonEmptyMap.of(one.fullName -> one, others.map(a => (a.fullName, a)): _*))
  }

  def fromFactsNel[A](facts: NonEmptyList[TypedFact[A]]): FactTypeSet[A] = fromNel(facts.map(_.typeInfo))

  def fromNel[A](types: NonEmptyList[FactType[A]]): FactTypeSet[A] = of(types.head, types.tail: _*)

  def fromNes[A](types: NonEmptySet[FactType[A]]): FactTypeSet[A] = of(types.head, types.tail.toSeq: _*)

  def validateSet[A](types: Set[FactType[A]]): Either[String, FactTypeSet[A]] = validateList(types.toList)

  def validateList[A](types: List[FactType[A]]): Either[String, FactTypeSet[A]] = {
    import cats.instances.string._
    val m = types.groupBy(_.fullName)
    val (duplicates, uniqueTypes) = m.partitionMap {
      case (name, one :: Nil) => Right((name, one))
      case (name, tooMany) => Left((name, tooMany))
    }
    if (duplicates.isEmpty) {
      NonEmptyMap
        .fromMap(immutable.SortedMap.from(uniqueTypes))
        .map(new FactTypeSet(_))
        .toRight("types list cannot be empty.")
    } else {
      Left(
        s"types list contains duplicate fact types: ${duplicates.map(_._1).mkString("'", "', '", "'")}",
      )
    }
  }

  def fromListOrThrow[A](types: List[FactType[A]]): FactTypeSet[A] = {
    validateList(types).fold(message => throw new IllegalArgumentException(message), identity)
  }
}
