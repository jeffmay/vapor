package com.rallyhealth

package vapors.data

import cats.Functor

/**
  * Represents a boundary on a 1-dimensional line at a specific position.
  *
  * It has two directions: [[Bounded.Below]] and [[Bounded.Above]].
  */
sealed trait Bounded[A]

object Bounded {

  final case class Above[A](
    lowerBound: A,
    inclusiveLowerBound: Boolean,
  ) extends Bounded[A]

  final case class Below[A](
    upperBound: A,
    inclusiveUpperBound: Boolean,
  ) extends Bounded[A]

  implicit final object AboveFunctorLike extends Functor[Above] {
    override def map[A, B](fa: Above[A])(f: A => B): Above[B] =
      fa.copy[B](lowerBound = f(fa.lowerBound))
  }

  implicit final object BelowFunctorLike extends Functor[Below] {
    override def map[A, B](fa: Below[A])(f: A => B): Below[B] =
      fa.copy[B](upperBound = f(fa.upperBound))
  }

}
