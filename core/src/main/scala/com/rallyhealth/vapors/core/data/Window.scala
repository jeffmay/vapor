package com.rallyhealth.vapors.core.data

import cats.data.Ior
import cats.{Invariant, Order, Show}

import scala.collection.immutable.NumericRange

trait Window[A] {
  def order: Order[A]
  def contains(value: A): Boolean
  def bounds: Ior[Bounded.Above[A], Bounded.Below[A]]
}

object Window {
  import Bounded._
  import cats.syntax.functor._
  import cats.syntax.order._
  import cats.syntax.show._

  implicit object InvariantWindow extends Invariant[Window] {
    override def imap[A, B](fa: Window[A])(f: A => B)(g: B => A): Window[B] = {
      implicit val o: Order[B] = Order.by(g)(fa.order)
      val newBounds = fa.bounds.bimap(a => a.map(f), b => b.map(f))
      KnownWindow(newBounds)
    }
  }

  def showWindowWithTerm[A : Show](term: String): Show[Window[A]] = Show.show { window =>
    import cats.instances.string._
    import cats.instances.tuple._
    val (op1, op2) = window.bounds
      .bimap(
        b => (if (b.inclusiveLowerBound) ">=" else ">", ""),
        b => ("", if (b.inclusiveUpperBound) "<=" else "<"),
      )
      .merge
    window.bounds match {
      case Ior.Left(lb) => s"$term $op1 ${lb.lowerBound.show}"
      case Ior.Right(ub) => s"$term $op2 ${ub.upperBound.show}"
      case Ior.Both(lb, ub) =>
        s"$term $op1 ${lb.lowerBound.show} and $term $op2 ${ub.upperBound.show}"
    }
  }

  implicit def showWindow[A : Show]: Show[Window[A]] = showWindowWithTerm("x")

  def fromRange(range: Range): Window[Int] = {
    import cats.instances.int._
    Window.between(range.start, includeMin = true, range.end, includeMax = range.isInclusive)
  }

  def fromRange[A : Order](range: NumericRange[A]): Window[A] = {
    Window.between(range.start, includeMin = true, range.end, includeMax = range.isInclusive)
  }

  def lessThan[A : Order](
    max: A,
    inclusive: Boolean,
  ): Window[A] = KnownWindow(Ior.Right(Below(max, inclusive)))

  def lessThan[A : Order](max: A): Window[A] = lessThan(max, inclusive = false)

  def lessThanOrEqual[A : Order](max: A): Window[A] = lessThan(max, inclusive = true)

  def greaterThan[A : Order](
    min: A,
    inclusive: Boolean,
  ): Window[A] = KnownWindow(Ior.Left(Above(min, inclusiveLowerBound = inclusive)))

  def greaterThan[A : Order](min: A): Window[A] = greaterThan(min, inclusive = false)

  def greaterThanOrEqual[A : Order](min: A): Window[A] = greaterThan(min, inclusive = true)

  def between[A : Order](
    min: A,
    includeMin: Boolean,
    max: A,
    includeMax: Boolean,
  ): Window[A] =
    KnownWindow(Ior.Both(Above(min, includeMin), Below(max, includeMax)))

  def between[A : Order](
    min: A,
    max: A,
  ): Window[A] = between(min, includeMin = true, max, includeMax = false)

  def betweenInclusive[A : Order](
    min: A,
    max: A,
  ): Window[A] = between(min, includeMin = true, max, includeMax = true)

  private[Window] final case class KnownWindow[A](
    bounds: Ior[Above[A], Below[A]],
  )(implicit
    override val order: Order[A],
  ) extends Window[A] {

    private val checkBetween: A => Boolean = bounds match {
      case Ior.Left(lb) if lb.inclusiveLowerBound => _ >= lb.lowerBound
      case Ior.Left(lb) => _ > lb.lowerBound

      case Ior.Right(ub) if ub.inclusiveUpperBound => _ <= ub.upperBound
      case Ior.Right(ub) => _ < ub.upperBound

      case Ior.Both(lb, ub) if lb.inclusiveLowerBound && ub.inclusiveUpperBound =>
        a => a >= lb.lowerBound && a <= ub.upperBound
      case Ior.Both(lb, ub) if ub.inclusiveUpperBound =>
        a => a > lb.lowerBound && a <= ub.upperBound
      case Ior.Both(lb, ub) if lb.inclusiveLowerBound =>
        a => a >= lb.lowerBound && a < ub.upperBound
      case Ior.Both(lb, ub) =>
        a => a > lb.lowerBound && a < ub.upperBound
    }

    override def contains(value: A): Boolean = checkBetween(value)
  }

}
