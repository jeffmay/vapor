package com.rallyhealth.vapors.core.dsl

import cats.data.NonEmptyList
import cats.free.FreeApplicative
import com.rallyhealth.vapors.core.{algebra, data, dsl}

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.{typeOf, TypeTag}

private[dsl] class FactFilterDsl {

  import algebra._
  import data._

  /**
    * An alias to this [[dsl]] object, so you can use infix operators and more easily explore
    * the list of supported expression builders.
    */
  final val __ = this

  /**
    * The root of all expression types.
    *
    * @tparam X the type of input
    * @tparam A the free parameter, used to describe the eventual output of the [[FreeApplicative]] functor
    */
  final type AnyExp[X, A] = FreeApplicative[ExpAlg[X, *], A]

  /**
    * An expression defined over all the given facts, regardless of type, that does not necessarily
    * terminate into a [[ResultSet]].
    */
  final type AnyFactExp[A] = FactsExp[Any, A]

  /**
    * An expression that terminates into a boolean, used for making a conditional query or filter.
    *
    * Useful for defining sub-expressions to [[whereAnyFactHas]], [[whereAllFactsHave]], [[exists]], [[all]], etc.
    */
  final type CondExp[X] = AnyExp[X, Boolean]

  /**
    * A useful alias for building a [[NamedLens]] by passing the identity lens as a starting point to a function.
    */
  final type FactLensId[T] = NamedLens.Id[Fact[T]]

  /**
    * A [[NamedLens]] defined over a [[Fact]].
    */
  final type FactLens[T, V] = NamedLens[Fact[T], V]

  /**
    * Alias for a non-empty list of facts of a certain type.
    */
  final type Facts[T] = NonEmptyList[Fact[T]]

  final type TerminalExp[T, V] = AnyExp[V, ResultSet[T]]

  /**
    * An expression that operates on a non-empty list of facts.
    *
    * @tparam T the type of fact
    * @tparam A the free parameter
    */
  type FactsExp[T, A] = AnyExp[Facts[T], A]

  /**
    * An expression that returns a [[ResultSet]] with metadata and all the facts used to compute the result.
    */
  final type TerminalFactsExp[T] = FactsExp[T, ResultSet[T]]

  // TODO: Use some cats typeclass instead of iterable?
  def all[F[x] <: IterableOnce[x], T, A](cond: CondExp[T]): CondExp[F[T]] = liftCondExp {
    ExpForAll[F[T], T, Boolean](identity[F[T]], cond, True, False)
  }

  // TODO: Use some cats typeclass instead of iterable?
  def exists[F[x] <: IterableOnce[x], T, A](cond: CondExp[T]): CondExp[F[T]] = liftCondExp {
    ExpExists[F[T], T, Boolean](identity[F[T]], cond, True, False)
  }

  def lessThan[T : Ordering](upperBound: T): CondExp[T] = liftCondExp {
    ExpWithin[T, Boolean](Window.lessThan(upperBound), True, False)
  }

  def lessThanOrEqual[T : Ordering](upperBound: T): CondExp[T] = liftCondExp {
    ExpWithin[T, Boolean](Window.lessThanOrEqual(upperBound), True, False)
  }

  def greaterThan[T : Ordering](lowerBound: T): CondExp[T] = liftCondExp {
    ExpWithin[T, Boolean](Window.greaterThan(lowerBound), True, False)
  }

  def greaterThanOrEqual[T : Ordering](lowerBound: T): CondExp[T] = liftCondExp {
    ExpWithin[T, Boolean](Window.greaterThanOrEqual(lowerBound), True, False)
  }

  def >[T : Ordering](lowerBound: T): CondExp[T] = greaterThan(lowerBound)
  def >=[T : Ordering](lowerBound: T): CondExp[T] = greaterThanOrEqual(lowerBound)
  def <[T : Ordering](upperBound: T): CondExp[T] = lessThan(upperBound)
  def <=[T : Ordering](upperBound: T): CondExp[T] = lessThanOrEqual(upperBound)

  // TODO: Figure out how to use this conditional tertiary expression

  def when[T, A](exp: AnyExp[T, Boolean])(thenExp: AnyExp[T, A])(elseExp: AnyExp[T, A]): AnyExp[T, A] = liftAnyExp {
    ExpCond[T, A](exp, thenExp, elseExp)
  }

  def withFactsOfType[U : ClassTag : TypeTag](factType: FactType[U]): TypedFactExpBuilder[Any, U] = {
    withFactsOfTypeIn[Any, U](FactTypeSet.of(factType))
  }

  def withFactsOfTypeIn[T >: U, U : ClassTag : TypeTag](factTypeSet: FactTypeSet[U]): TypedFactExpBuilder[T, U] = {
    new TypedFactExpBuilder[T, U](factTypeSet)
  }

  def and[T, A : Intersect](
    one: AnyExp[T, A],
    two: AnyExp[T, A],
    others: AnyExp[T, A]*,
  ): AnyExp[T, A] = liftAnyExp {
    ExpAnd[T, A](
      Intersect[A].intersect,
      one :: two :: others.toList,
    )
  }

  def or[T, A : Union](
    one: AnyExp[T, A],
    two: AnyExp[T, A],
    others: AnyExp[T, A]*,
  ): AnyExp[T, A] = liftAnyExp {
    ExpOr[T, A](
      Union[A].union,
      one :: two :: others.toList,
    )
  }

  implicit def logicalOps[T, A](exp: AnyExp[T, A]): LogicalOps[T, A] = new LogicalOps(exp)

  def typeNameOf[T : TypeTag]: String = typeOf[T].toString.split('.').dropWhile(_.charAt(0).isLower).mkString(".")

  def alwaysTrue[T]: CondExp[T] = liftCondExp(ExpPure("True", _ => true))

  def alwaysFalse[T]: CondExp[T] = liftCondExp(ExpPure("False", _ => false))

  def alwaysMatch: TerminalFactsExp[Any] = liftTermExp(ExpPure("AlwaysMatch", FactsMatch(_)))

  def alwaysEmpty: TerminalFactsExp[Any] = liftTermExp(ExpPure("AlwaysEmpty", _ => NoFactsMatch()))

  // Helper methods for building conditional expressions that terminate in a functor to boolean
  private def True[X]: X => Boolean = _ => true
  private def False[X]: X => Boolean = _ => false

  /** @see [[TerminalFactsExp]] */
  private def liftTermExp[T](value: ExpAlg[Facts[T], ResultSet[T]]): TerminalFactsExp[T] = FreeApplicative.lift(value)

  /** @see [[FactsExp]] */
  private def liftFactsExp[T, A](value: ExpAlg[Facts[T], A]): FactsExp[T, A] = FreeApplicative.lift(value)

  /** @see [[CondExp]] */
  private def liftCondExp[X](value: ExpAlg[X, Boolean]): CondExp[X] = FreeApplicative.lift(value)

  /** @see [[AnyExp]] */
  private def liftAnyExp[X, A](value: ExpAlg[X, A]): AnyExp[X, A] = FreeApplicative.lift(value)
}
