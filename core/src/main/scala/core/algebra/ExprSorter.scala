package com.rallyhealth.vapors

package core.algebra

import core.lens.NamedLens

import cats.Order
import cats.syntax.contravariant._
import cats.syntax.show._

import scala.collection.Factory
import scala.reflect.runtime.universe.{typeOf, TypeTag}

sealed trait ExprSorter[M[_], R] extends (M[R] => M[R]) {

  def sortDescription: String

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
