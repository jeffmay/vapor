package com.rallyhealth.vapors.core.dsl

import cats.data.NonEmptyList
import com.rallyhealth.vapors.core.algebra.{CaptureP, ConditionBranch, Expr}

final class WhenExprBuilder[V, P](private val whenExpr: CondExpr[V, P]) extends AnyVal {

  def thenReturn[R](thenExpr: Expr[V, R, P]): WhenElseExprBuilder[V, R, P] =
    new WhenElseExprBuilder(NonEmptyList.of(ConditionBranch(whenExpr, thenExpr)))
}

final class WhenElifExprBuilder[V, R, P](private val t: (CondExpr[V, P], NonEmptyList[ConditionBranch[V, R, P]]))
  extends AnyVal {

  def thenReturn(thenExpr: Expr[V, R, P]): WhenElseExprBuilder[V, R, P] =
    new WhenElseExprBuilder(ConditionBranch(t._1, thenExpr) :: t._2)
}

final class WhenElseExprBuilder[V, R, P](private val branches: NonEmptyList[ConditionBranch[V, R, P]]) extends AnyVal {

  def elseReturn(
    elseExpr: Expr[V, R, P],
  )(implicit
    captureResult: CaptureP[V, R, P],
  ): Expr.When[V, R, P] =
    Expr.When(branches.reverse.toNev, elseExpr, captureResult)

  def elif(elifExpr: CondExpr[V, P]): WhenElifExprBuilder[V, R, P] = new WhenElifExprBuilder((elifExpr, branches))
}
