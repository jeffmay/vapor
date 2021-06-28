package com.rallyhealth

package vapors.dsl

import vapors.algebra.{CaptureP, Expr}
import vapors.lens.NamedLens

import cats.MonoidK

import scala.collection.Factory

final class ConcatOutputExprBuilder[V, M[_], R, P](private val expressions: LazyList[Expr[V, M[R], P]]) extends AnyVal {

  /**
    * Concatenates the results of all the expressions into a single [[LazyList]].
    *
    * This requires that the return type is an [[IterableOnce]] scala collection.
    *
    * This is generally preferable as a data structure because you probably don't want to compute things
    * that are not used.
    *
    * @example
    * {{{
    *   assert(eval(concat(Some(1), None, Some(2)).toLazyList).output.value == LazyList(1, 2))
    * }}}
    */
  def toLazyList(
    implicit
    ev: M[R] <:< IterableOnce[R],
    captureResult: CaptureP[V, LazyList[R], P],
  ): Expr.ConcatOutput[V, LazyList, R, P] =
    Expr.ConcatOutput(
      expressions.map { expr =>
        Expr.SelectFromOutput(
          expr,
          NamedLens.id[M[R]].asIterable[R].to(LazyList: Factory[R, LazyList[R]]),
          captureResult,
        )
      },
      captureResult,
    )

  /**
    * Combines all the results of the expressions using the definition of monoid for the return type.
    *
    * This requires that the return type constructor has a standard definition for [[MonoidK]].
    *
    * @example
    * {{{
    *   assert(eval(concat(List(1, 2), List(3, 4)).toOutputMonoid).output.value == List(1, 2, 3, 4))
    * }}}
    */
  def toOutputMonoid(
    implicit
    monoidKM: MonoidK[M],
    captureResult: CaptureP[V, M[R], P],
  ): Expr.ConcatOutput[V, M, R, P] =
    Expr.ConcatOutput(expressions, captureResult)

}

object ConcatOutputExprBuilder {

  implicit def defaultAsMonoid[V, M[_] : MonoidK, R, P](
    builder: ConcatOutputExprBuilder[V, M, R, P],
  )(implicit
    captureResult: CaptureP[V, M[R], P],
  ): Expr.ConcatOutput[V, M, R, P] =
    builder.toOutputMonoid
}
