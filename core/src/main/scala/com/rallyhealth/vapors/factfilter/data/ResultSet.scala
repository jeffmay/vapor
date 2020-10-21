package com.rallyhealth.vapors.factfilter.data

import cats.data.NonEmptyList
import com.rallyhealth.vapors.core.logic.{Intersect, Union}

/**
  * The result of evaluating an [[com.rallyhealth.vapors.factfilter.dsl.Exp]]
  */
sealed abstract class ResultSet extends Equals {

  /**
    * A subset of the given facts that can be used to prove the logical query to be true.
    */
  def matchingFacts: List[Fact]

  /**
    * True when [[matchingFacts]] is empty, and there is no subset of the facts provided that
    * * can sufficiently prove the query to be true.
    *
    * @see [[isFalse]]
    */
  final def isEmpty: Boolean = matchingFacts.isEmpty

  /**
    * True when [[matchingFacts]] is not empty.
    *
    * @see [[isTrue]]
    */
  @inline final def nonEmpty: Boolean = !isEmpty

  /**
    * True when [[matchingFacts]] is not empty, and there exists a subset of the provided facts
    * that can be used to prove the query to be true.
    */
  @inline final def isTrue: Boolean = !isEmpty

  /**
    * True when [[matchingFacts]] is empty, and there is no subset of the facts provided that
    * can sufficiently prove the query to be true.
    */
  @inline final def isFalse: Boolean = isEmpty

  /**
    * Same as [[matchingFacts]], but as a Set.
    */
  lazy val matchingFactSet: Set[Fact] = matchingFacts.toSet

  /**
    * Union of the two sets of results.
    */
  final def union(o: ResultSet): ResultSet = {
    if (o.matchingFactSet == this.matchingFactSet) o
    else
      this match {
        case NoFactsMatch() => o
        case FactsMatch(matchingFactsNel) =>
          val newFacts = o.matchingFacts.collect {
            case fact if !matchingFactSet.contains(fact) => fact
          }
          FactsMatch(matchingFactsNel ++ newFacts)
      }
  }

  /**
    * Alias for [[union]].
    */
  final def ++(o: ResultSet): ResultSet = this.union(o)

  /**
    * Alias for [[union]].
    */
  @inline final def |(o: ResultSet): ResultSet = this.union(o)

  // SAFE: ResultSet is sealed, so all subclasses are matched on by this parent type.
  def canEqual(other: Any): Boolean = other.isInstanceOf[ResultSet]

  override def equals(other: Any): Boolean = other match {
    case that: ResultSet =>
      that.canEqual(this) &&
        this.matchingFacts == that.matchingFacts
    case _ => false
  }

  override def hashCode(): Int = {
    val state = matchingFacts
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override final def toString: String = this match {
    case NoFactsMatch() => "NoFactsMatch()"
    case matched: FactsMatch => s"FactsMatch(${matched.matchingFactsNel})"
  }
}

object ResultSet {

  def apply(facts: List[Fact]): ResultSet = {
    NonEmptyList.fromList(facts).map(FactsMatch(_)).getOrElse(NoFactsMatch())
  }

  def fromNel(nonEmptyFacts: NonEmptyList[Fact]): FactsMatch = FactsMatch(nonEmptyFacts)

  implicit val unionAbstractResultSet: Union[ResultSet] = {
    _.reduceLeft[ResultSet](_ | _) // union all the possible facts (for better quality calculations)
  }

  implicit val intersectAbstractResultSet: Intersect[ResultSet] = {
    _.reduceLeft[ResultSet] {
      case (NoFactsMatch(), _) | (_, NoFactsMatch()) => NoFactsMatch() // skip the all expressions if the first failed
      case (acc, nextResult) => acc | nextResult // union all the required facts
    }
  }
}

/**
  * Extends [[ResultSet]] to provide more specific return types.
  */
sealed trait TypedResultSet[A] extends ResultSet {

  /**
    * Same as [[ResultSet.matchingFacts]], but with [[TypedFact]]s.
    *
    * @see [[ResultSet.matchingFacts]]
    */
  override def matchingFacts: List[TypedFact[A]]
}

object TypedResultSet {

  def apply[A](facts: List[TypedFact[A]]): TypedResultSet[A] = {
    NonEmptyList.fromList(facts).map(FactsMatch(_)).getOrElse(NoFactsMatch())
  }

  def fromNel[A](nonEmptyFacts: NonEmptyList[TypedFact[A]]): TypedFactsMatch[A] = FactsMatch(nonEmptyFacts)

  implicit def union[T]: Union[TypedResultSet[T]] = _.foldLeft(TypedResultSet[T](Nil)) {
    case (NoFactsMatch(), next) => next
    case (acc, next) if acc.matchingFactSet == next.matchingFactSet => acc
    case (acc, NoFactsMatch()) => acc
    case (acc, TypedFactsMatch(b)) =>
      val newFacts = b.collect {
        case fact if !acc.matchingFactSet.contains(fact) => fact
      }
      TypedResultSet(acc.matchingFacts ::: newFacts)
  }
}

// TODO: Should this be protected?
sealed class FactsMatch(val matchingFactsNel: NonEmptyList[Fact]) extends ResultSet {

  /**
    * @see [[ResultSet.matchingFacts]]
    */
  override def matchingFacts: List[Fact] = matchingFactsNel.toList
}

object FactsMatch {

  def apply[T](matchingFactsNel: FactsOfType[T]): TypedFactsMatch[T] = {
    new TypedFactsMatch(matchingFactsNel)
  }

  def apply(matchingFactsNel: Facts): FactsMatch = {
    new FactsMatch(matchingFactsNel)
  }

  def unapply(matching: FactsMatch): Some[Facts] = {
    Some(matching.matchingFactsNel)
  }
}

final class TypedFactsMatch[A](override val matchingFactsNel: NonEmptyList[TypedFact[A]])
  extends FactsMatch(matchingFactsNel)
  with TypedResultSet[A] {

  /**
    * Same as [[ResultSet.matchingFacts]], but with [[TypedFact]]s.
    *
    * @see [[ResultSet.matchingFacts]]
    */
  override def matchingFacts: List[TypedFact[A]] = matchingFactsNel.toList
}

object TypedFactsMatch {

  def apply[A](matching: FactsOfType[A]): TypedFactsMatch[A] = new TypedFactsMatch(matching)

  def unapply[A](resultSet: TypedResultSet[A]): Option[FactsOfType[A]] = resultSet match {
    case NoFactsMatch() => None
    case factsMatch: TypedFactsMatch[A] => Some(factsMatch.matchingFactsNel)
  }
}

case object NoFactsMatch extends ResultSet with TypedResultSet[Nothing] {

  /**
    * @see [[ResultSet.matchingFacts]]
    */
  override def matchingFacts: List[TypedFact[Nothing]] = Nil

  /**
    * For forwards compatibility for when this might become a case class.
    */
  def apply[T](): TypedResultSet[T] = this.asInstanceOf[TypedResultSet[T]]
  def unapply(res: ResultSet): Boolean = res == this
}
