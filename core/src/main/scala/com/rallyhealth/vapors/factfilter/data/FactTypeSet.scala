package com.rallyhealth.vapors.factfilter.data

import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet}

import scala.collection.immutable.{SortedMap, SortedSet}

final case class FactTypeSet[A] private (typeMap: NonEmptyMap[String, FactType[A]]) extends AnyVal {

  def typeList: NonEmptyList[FactType[A]] = NonEmptyList.fromListUnsafe(typeMap.toSortedMap.values.toList)

  def typeSet: NonEmptySet[FactType[A]] = NonEmptySet.fromSetUnsafe(SortedSet.from(typeMap.toSortedMap.values))

  def unapply(fact: Fact): Option[TypedFact[A]] = collector.lift(fact)

  /**
    * Checks the [[FactType]] for equality with one of the types in this set.
    *
    * @return the [[TypedFact]] as the expected type.
    */
  def cast(fact: Fact): Option[TypedFact[A]] = collector.lift(fact)

  /**
    *
    * @return a partial function for collecting facts of a specific type
    */
  def collector: PartialFunction[Fact, TypedFact[A]] = {
    // Justification: This checks equality of the FactType at runtime, so it should be safe to cast
    case fact @ TypedFact(factType, _) if typeMap(factType.fullName).contains(factType) =>
      fact.asInstanceOf[TypedFact[A]]
  }

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
    val m = types.groupBy(_.fullName)
    val (duplicates, uniqueTypes) = m.partitionMap {
      case (name, one :: Nil) => Right((name, one))
      case (name, tooMany) => Left((name, tooMany))
    }
    if (duplicates.isEmpty) {
      NonEmptyMap
        .fromMap(SortedMap.from(uniqueTypes))
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
