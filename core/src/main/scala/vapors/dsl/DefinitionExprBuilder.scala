package com.rallyhealth

package vapors.dsl

import vapors.algebra.Expr
import vapors.data.{FactSet, FactType}

import cats.{Foldable, Id}

final class DefinitionExprBuilder[T](private val factType: FactType[T]) extends AnyVal {

  /**
    * Creates a definition that always adds exactly one fact.
    */
  def from[P](
    defExpr: RootExpr[T, P],
  )(implicit
    captureResult: CaptureRootExpr[FactSet, P],
  ): Expr.Define[Id, T, P] =
    Expr.Define[Id, T, P](factType, defExpr, captureResult)

  /**
    * Creates a definition that adds a fact for every value returned by the [[Foldable]] [[Expr]].
    */
  def fromEvery[M[_] : Foldable, P](
    defExpr: RootExpr[M[T], P],
  )(implicit
    captureResult: CaptureRootExpr[FactSet, P],
  ): Expr.Define[M, T, P] =
    Expr.Define(factType, defExpr, captureResult)
}
