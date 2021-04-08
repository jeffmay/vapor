package com.rallyhealth.vapors

package core.dsl

import core.algebra.{CaptureP, Expr, ExprConverter, NonEmptyExprHList}

import cats.{Align, FunctorFilter}
import shapeless.{::, HList, HNil}

trait WrapEachExprSyntax {

  def wrapEach[V, M[_], E1, E2, P](
    e1: Expr[V, M[E1], P],
    e2: Expr[V, M[E2], P],
  ): WrapEachExprHListWrapper[V, M, E1 :: E2 :: HNil, P] =
    new WrapEachExprHListWrapper(e1 :: NonEmptyExprHList.tailK(e2))

  def wrapEach[V, M[_], E1, E2, E3, P](
    e1: Expr[V, M[E1], P],
    e2: Expr[V, M[E2], P],
    e3: Expr[V, M[E3], P],
  ): WrapEachExprHListWrapper[V, M, E1 :: E2 :: E3 :: HNil, P] =
    new WrapEachExprHListWrapper(e1 :: e2 :: NonEmptyExprHList.tailK(e3))

  def wrapEach[V, M[_], E1, E2, E3, E4, P](
    e1: Expr[V, M[E1], P],
    e2: Expr[V, M[E2], P],
    e3: Expr[V, M[E3], P],
    e4: Expr[V, M[E4], P],
  ): WrapEachExprHListWrapper[V, M, E1 :: E2 :: E3 :: E4 :: HNil, P] =
    new WrapEachExprHListWrapper(e1 :: e2 :: e3 :: NonEmptyExprHList.tailK(e4))

  def wrapEach[V, M[_], E1, E2, E3, E4, E5, P](
    e1: Expr[V, M[E1], P],
    e2: Expr[V, M[E2], P],
    e3: Expr[V, M[E3], P],
    e4: Expr[V, M[E4], P],
    e5: Expr[V, M[E5], P],
  ): WrapEachExprHListWrapper[V, M, E1 :: E2 :: E3 :: E4 :: E5 :: HNil, P] =
    new WrapEachExprHListWrapper(e1 :: e2 :: e3 :: e4 :: NonEmptyExprHList.tailK(e5))

  def wrapEach[V, M[_], E1, E2, E3, E4, E5, E6, P](
    e1: Expr[V, M[E1], P],
    e2: Expr[V, M[E2], P],
    e3: Expr[V, M[E3], P],
    e4: Expr[V, M[E4], P],
    e5: Expr[V, M[E5], P],
    e6: Expr[V, M[E6], P],
  ): WrapEachExprHListWrapper[V, M, E1 :: E2 :: E3 :: E4 :: E5 :: E6 :: HNil, P] =
    new WrapEachExprHListWrapper(e1 :: e2 :: e3 :: e4 :: e5 :: NonEmptyExprHList.tailK(e6))

  def wrapEach[V, M[_], E1, E2, E3, E4, E5, E6, E7, P](
    e1: Expr[V, M[E1], P],
    e2: Expr[V, M[E2], P],
    e3: Expr[V, M[E3], P],
    e4: Expr[V, M[E4], P],
    e5: Expr[V, M[E5], P],
    e6: Expr[V, M[E6], P],
    e7: Expr[V, M[E7], P],
  ): WrapEachExprHListWrapper[V, M, E1 :: E2 :: E3 :: E4 :: E5 :: E6 :: E7 :: HNil, P] =
    new WrapEachExprHListWrapper(e1 :: e2 :: e3 :: e4 :: e5 :: e6 :: NonEmptyExprHList.tailK(e7))

  def wrapEach[V, M[_], E1, E2, E3, E4, E5, E6, E7, E8, P](
    e1: Expr[V, M[E1], P],
    e2: Expr[V, M[E2], P],
    e3: Expr[V, M[E3], P],
    e4: Expr[V, M[E4], P],
    e5: Expr[V, M[E5], P],
    e6: Expr[V, M[E6], P],
    e7: Expr[V, M[E7], P],
    e8: Expr[V, M[E8], P],
  ): WrapEachExprHListWrapper[V, M, E1 :: E2 :: E3 :: E4 :: E5 :: E6 :: E7 :: E8 :: HNil, P] =
    new WrapEachExprHListWrapper(e1 :: e2 :: e3 :: e4 :: e5 :: e6 :: e7 :: NonEmptyExprHList.tailK(e8))

  def wrapEach[V, M[_], E1, E2, E3, E4, E5, E6, E7, E8, E9, P](
    e1: Expr[V, M[E1], P],
    e2: Expr[V, M[E2], P],
    e3: Expr[V, M[E3], P],
    e4: Expr[V, M[E4], P],
    e5: Expr[V, M[E5], P],
    e6: Expr[V, M[E6], P],
    e7: Expr[V, M[E7], P],
    e8: Expr[V, M[E8], P],
    e9: Expr[V, M[E9], P],
  ): WrapEachExprHListWrapper[V, M, E1 :: E2 :: E3 :: E4 :: E5 :: E6 :: E7 :: E8 :: E9 :: HNil, P] =
    new WrapEachExprHListWrapper(e1 :: e2 :: e3 :: e4 :: e5 :: e6 :: e7 :: e8 :: NonEmptyExprHList.tailK(e9))

  def wrapEach[V, M[_], E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, P](
    e1: Expr[V, M[E1], P],
    e2: Expr[V, M[E2], P],
    e3: Expr[V, M[E3], P],
    e4: Expr[V, M[E4], P],
    e5: Expr[V, M[E5], P],
    e6: Expr[V, M[E6], P],
    e7: Expr[V, M[E7], P],
    e8: Expr[V, M[E8], P],
    e9: Expr[V, M[E9], P],
    e10: Expr[V, M[E10], P],
  ): WrapEachExprHListWrapper[V, M, E1 :: E2 :: E3 :: E4 :: E5 :: E6 :: E7 :: E8 :: E9 :: E10 :: HNil, P] =
    new WrapEachExprHListWrapper(e1 :: e2 :: e3 :: e4 :: e5 :: e6 :: e7 :: e8 :: e9 :: NonEmptyExprHList.tailK(e10))

  def wrapEach[V, M[_], E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, P](
    e1: Expr[V, M[E1], P],
    e2: Expr[V, M[E2], P],
    e3: Expr[V, M[E3], P],
    e4: Expr[V, M[E4], P],
    e5: Expr[V, M[E5], P],
    e6: Expr[V, M[E6], P],
    e7: Expr[V, M[E7], P],
    e8: Expr[V, M[E8], P],
    e9: Expr[V, M[E9], P],
    e10: Expr[V, M[E10], P],
    e11: Expr[V, M[E11], P],
  ): WrapEachExprHListWrapper[V, M, E1 :: E2 :: E3 :: E4 :: E5 :: E6 :: E7 :: E8 :: E9 :: E10 :: E11 :: HNil, P] =
    new WrapEachExprHListWrapper(
      e1 :: e2 :: e3 :: e4 :: e5 :: e6 :: e7 :: e8 :: e9 :: e10 :: NonEmptyExprHList.tailK(e11),
    )

  def wrapEach[V, M[_], E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, P](
    e1: Expr[V, M[E1], P],
    e2: Expr[V, M[E2], P],
    e3: Expr[V, M[E3], P],
    e4: Expr[V, M[E4], P],
    e5: Expr[V, M[E5], P],
    e6: Expr[V, M[E6], P],
    e7: Expr[V, M[E7], P],
    e8: Expr[V, M[E8], P],
    e9: Expr[V, M[E9], P],
    e10: Expr[V, M[E10], P],
    e11: Expr[V, M[E11], P],
    e12: Expr[V, M[E12], P],
  ): WrapEachExprHListWrapper[
    V,
    M,
    E1 :: E2 :: E3 :: E4 :: E5 :: E6 :: E7 :: E8 :: E9 :: E10 :: E11 :: E12 :: HNil,
    P,
  ] =
    new WrapEachExprHListWrapper(
      e1 :: e2 :: e3 :: e4 :: e5 :: e6 :: e7 :: e8 :: e9 :: e10 :: e11 :: NonEmptyExprHList.tailK(e12),
    )
}

final class WrapEachExprHListWrapper[V, M[_], L <: HList, P](private val exprHList: NonEmptyExprHList[V, M, L, P])
  extends AnyVal {

  def zippedToShortest(
    implicit
    alignM: Align[M],
    filterM: FunctorFilter[M],
  ): ZippedToShortestExprWrapper[V, M, L, P] = new ZippedToShortestExprWrapper(exprHList)
}

final class ZippedToShortestExprWrapper[V, M[_] : Align : FunctorFilter, L <: HList, P](
  override protected val exprHList: NonEmptyExprHList[V, M, L, P],
) extends HListOperationWrapper[V, M, L, P] {

  override type Op[R] = Expr.ZipOutput[V, M, L, R, P]

  override protected def buildOp[R](
    converter: ExprConverter[L, R],
    captureResult: CaptureP[V, M[R], P],
  ): Expr.ZipOutput[V, M, L, R, P] =
    Expr.ZipOutput(exprHList, converter, captureResult)
}
