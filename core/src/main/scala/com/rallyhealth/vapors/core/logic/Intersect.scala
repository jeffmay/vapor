package com.rallyhealth.vapors.core.logic

import cats.{Invariant, Monoid}

// TODO: Remove this redundant typeclass. Use Disjunction instead.
trait Intersect[A] {

  def intersect(results: Seq[A]): A

  def monoid: Monoid[A] = {
    new Monoid[A] {
      override def empty: A = intersect(Nil)
      override def combine(
        x: A,
        y: A,
      ): A = intersect(x :: y :: Nil)
    }
  }
}

object Intersect {

  def apply[A](implicit A: Intersect[A]): Intersect[A] = A

  implicit object FunctorInstances extends Invariant[Intersect] {
    override def imap[A, B](fa: Intersect[A])(f: A => B)(g: B => A): Intersect[B] = { results =>
      f(fa.intersect(results.map(g)))
    }
  }

  implicit object boolean extends Intersect[Boolean] {
    override def intersect(results: Seq[Boolean]): Boolean = results.forall(identity)
  }

  implicit def set[A]: Intersect[Set[A]] = _.reduce(_ & _)

}
