package com.rallyhealth.vapors.core.logic

import cats.Monoid

// TODO: Remove this redundant typeclass. Use Disjunction instead.
trait Union[A] {

  def union(results: Seq[A]): A

  def monoid: Monoid[A] = {
    new Monoid[A] {
      override def empty: A = union(Nil)
      override def combine(
        x: A,
        y: A,
      ): A = union(x :: y :: Nil)
    }
  }
}

object Union {

  def apply[A](implicit A: Union[A]): Union[A] = A

  implicit object boolean extends Union[Boolean] {
    override def union(results: Seq[Boolean]): Boolean = results.exists(identity)
  }

  implicit def set[A]: Union[Set[A]] = _.reduce(_ | _)

  implicit def fromMonoid[A : Monoid]: Union[A] = {
    import cats.syntax.monoid._
    _.reduce { _ |+| _ }
  }
}
