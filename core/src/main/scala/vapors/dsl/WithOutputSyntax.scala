package com.rallyhealth

package vapors.dsl

import vapors.algebra.Expr

import cats.Foldable

trait WithOutputSyntax {

  implicit def thenChainValue[V, R, P](expr: Expr[V, R, P]): WithOutputValueExprBuilder[V, R, P] =
    new WithOutputValueExprBuilder(expr)

  implicit def thenChainFoldable[V, M[_], R, P](expr: Expr[V, M[R], P]): WithOutputFoldableExprBuilder[V, M, R, P] =
    new WithOutputFoldableExprBuilder(expr)

}

// TODO: These classes would not be needed if everything returned builders
final class WithOutputValueExprBuilder[V, R, P](private val expr: Expr[V, R, P]) extends AnyVal {

  /**
    * Converts any [[Expr]] node into a [[ValExprBuilder]].
    */
  def withOutputValue: ValExprBuilder[V, R, P] = new ValExprBuilder(expr)
}

final class WithOutputFoldableExprBuilder[V, M[_], R, P](private val expr: Expr[V, M[R], P]) extends AnyVal {

  /**
    * Converts any [[Expr]] for which there is a [[Foldable]] definition for the return type constructor
    * into a [[FoldableExprBuilder]].
    */
  def withOutputFoldable(
    implicit
    foldableM: Foldable[M],
  ): FoldableExprBuilder[V, M, R, P] = new FoldableExprBuilder(expr)
}
