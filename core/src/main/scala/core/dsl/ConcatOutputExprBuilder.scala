package com.rallyhealth.vapors

package core.dsl

import core.algebra.{CaptureP, Expr}
import core.lens.NamedLens

import cats.MonoidK

import scala.collection.Factory

final class ConcatOutputExprBuilder[V, M[_], R, P](private val expressions: LazyList[Expr[V, M[R], P]]) extends AnyVal {

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
