package com.rallyhealth

package vapors.dsl

import vapors.algebra.{CaptureP, Expr}
import vapors.data.{FactTypeSet, TypedFact}

/**
  * @note this is not a value class because the input [[FactTypeSet]] is also a value class AND the [[CaptureP]]
  *       implicit is needed in the original [[withFactsOfType]] method in order for type inference to work properly.
  *       We could ask for the implicit again, but that is confusing for the caller who would have to look at the
  *       source code to know which instance is actually used.
  */
final class WithFactsOfTypeBuilder[T, P](
  factTypeSet: FactTypeSet[T],
)(implicit
  captureInput: CaptureFromFacts[T, P],
) {

  def where[M[_], U](
    buildSubExpr: ExprBuilder.FoldableFn[Seq, TypedFact[T], M, U, P],
  )(implicit
    captureResult: CaptureRootExpr[M[U], P],
  ): Expr.WithFactsOfType[T, M[U], P] =
    Expr.WithFactsOfType(
      factTypeSet,
      buildSubExpr(new FoldableExprBuilder(input)).returnOutput,
      captureResult,
    )

  def returnInput(
    implicit
    captureInput: CaptureFromFacts[T, P],
    captureResult: CaptureRootExpr[Seq[TypedFact[T]], P],
  ): Expr.WithFactsOfType[T, Seq[TypedFact[T]], P] =
    Expr.WithFactsOfType(factTypeSet, input(captureInput), captureResult)
}
