package com.rallyhealth.vapors.factfilter.data

import cats.data.NonEmptyList
import cats.{Eq, Monoid}
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
    * can sufficiently prove the query to be true.
    */
  final def isEmpty: Boolean = matchingFacts.isEmpty

  /**
    * True when [[matchingFacts]] is not empty, and there exists a subset of the provided facts
    * that can be used to prove the query to be true.
    */
  @inline final def nonEmpty: Boolean = !isEmpty

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
  @inline final def ++(o: ResultSet): ResultSet = this.union(o)

  /**
    * Alias for [[union]].
    */
  @inline final def |(o: ResultSet): ResultSet = this.union(o)

  // SAFE: ResultSet is sealed, so all subclasses are matched on by this parent type.
  def canEqual(other: Any): Boolean = other.isInstanceOf[ResultSet]

  override def equals(other: Any): Boolean = other match {
    case that: ResultSet =>
      that.canEqual(this) &&
        this.matchingFactSet == that.matchingFactSet
    case _ => false
  }

  override def hashCode(): Int = matchingFactSet.hashCode()

  override final def toString: String = this match {
    case NoFactsMatch() => "NoFactsMatch()"
    case matched: FactsMatch => s"FoundEvidence(${matched.matchingFactsNel})"
  }
}

object ResultSet {

  def apply(facts: List[Fact]): ResultSet = {
    NonEmptyList.fromList(facts).map(FactsMatch(_)).getOrElse(NoFactsMatch())
  }

  def fromNel(nonEmptyFacts: NonEmptyList[Fact]): FactsMatch = FactsMatch(nonEmptyFacts)

  /**
    * This is safe if you are treating a [[ResultSet]] as a collection of facts.
    *
    * However, if you want to perform logical operations for combining [[ResultSet]]s, you should use
    * a separate type-class, like [[Union]] or [[Intersect]].
    *
    * @see [[com.rallyhealth.vapors.factfilter.evaluator.InterpretExprAsFunction.Output.monoid]]
    */
  implicit val monoid: Monoid[ResultSet] = {
    new Monoid[ResultSet] {
      override def empty: ResultSet = NoFactsMatch
      override def combine(
        x: ResultSet,
        y: ResultSet,
      ): ResultSet = x ++ y
    }
  }

  implicit val union: Union[ResultSet] = {
    _.reduceLeft[ResultSet](_ | _) // union all the possible facts (for better quality calculations)
  }

  implicit val intersect: Intersect[ResultSet] = {
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

  def empty[A]: TypedResultSet[A] = NoFactsMatch()

  def apply[A](facts: List[TypedFact[A]]): TypedResultSet[A] = {
    NonEmptyList.fromList(facts).map(TypedFactsMatch(_)).getOrElse(NoFactsMatch())
  }

  def fromNel[A](nonEmptyFacts: NonEmptyList[TypedFact[A]]): TypedFactsMatch[A] = TypedFactsMatch(nonEmptyFacts)

  implicit def eq[T]: Eq[TypedResultSet[T]] = Eq.fromUniversalEquals

  private val monoidAny: Monoid[TypedResultSet[Any]] = {
    new Monoid[TypedResultSet[Any]] {
      override def empty: TypedResultSet[Any] = NoFactsMatch()
      override def combine(
        x: TypedResultSet[Any],
        y: TypedResultSet[Any],
      ): TypedResultSet[Any] = {
        if (x.matchingFactSet == y.matchingFactSet) x
        else
          x match {
            case NoFactsMatch() => y
            case TypedFactsMatch(matchingFactsNel) =>
              val newFacts = matchingFactsNel.collect {
                case fact if !x.matchingFactSet.contains(fact) => fact
              }
              TypedFactsMatch(matchingFactsNel ++ newFacts)
          }
      }
    }
  }

  implicit def monoid[T]: Monoid[TypedResultSet[T]] = monoidAny.asInstanceOf[Monoid[TypedResultSet[T]]]

  implicit def intersect[T]: Intersect[TypedResultSet[T]] = _.reduce { (x, y) =>
    import cats.syntax.eq._
    import cats.syntax.monoid._
    if (x === y) x
    else if (x.isEmpty) x
    else if (y.isEmpty) y
    else x |+| y
  }
}

sealed class FactsMatch protected (val matchingFactsNel: NonEmptyList[Fact]) extends ResultSet {

  /**
    * @see [[ResultSet.matchingFacts]]
    */
  override def matchingFacts: List[Fact] = matchingFactsNel.toList
}

object FactsMatch {

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
