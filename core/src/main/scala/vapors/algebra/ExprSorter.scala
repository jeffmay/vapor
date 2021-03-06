package com.rallyhealth

package vapors.algebra

import vapors.lens.NamedLens

import cats.Order
import cats.syntax.contravariant._
import cats.syntax.show._

import scala.collection.Factory
import scala.reflect.runtime.universe.{typeOf, TypeTag}

/**
  * Defines a closed set of operations for sorting elements of the given arity-1 type constructor.
  *
  * This is basically like an [[Order]], except it exists only for serialization and debugging purposes.
  */
sealed trait ExprSorter[M[_], R] extends (M[R] => M[R]) {

  /**
    * A description of how the elements are sorted for serialization and debugging purposes.
    */
  def sortDescription: String

  /**
    * Sort the given collection.
    */
  override def apply(collection: M[R]): M[R]
}

object ExprSorter {

  private final class Impl[M[_], R](
    override val sortDescription: String,
    op: M[R] => M[R],
  ) extends ExprSorter[M, R] {
    override def apply(collection: M[R]): M[R] = op(collection)
  }

  def byNaturalOrder[M[_], R : Order : TypeTag](
    implicit
    ev: M[R] <:< Seq[R],
    factory: Factory[R, M[R]],
  ): ExprSorter[M, R] =
    new Impl(s"natural order of ${typeOf[R]}", _.sorted(Order[R].toOrdering).to(factory))

  def byField[M[_], S : TypeTag, R : Order](
    lens: NamedLens[S, R],
  )(implicit
    ev: M[S] <:< Seq[S],
    factory: Factory[S, M[S]],
  ): ExprSorter[M, S] =
    new Impl(
      s"sort using ${typeOf[S]}${lens.path.show}",
      _.sorted(Order[R].contramap(lens.get).toOrdering).to(factory),
    )

}
