package com.rallyhealth.vapors

package core.dsl

import core.algebra.{CaptureP, Expr, ExprConverter, NonEmptyExprHList}

import cats.Id
import shapeless.{::, Generic, HList, HNil}

trait WrapExprSyntax {

  def wrap[V, E1, P](e1: Expr[V, E1, P]): ExprHListWrapper[V, E1 :: HNil, P] =
    new ExprHListWrapper(NonEmptyExprHList.tail(e1))

  def wrap[V, E1, E2, P](
    e1: Expr[V, E1, P],
    e2: Expr[V, E2, P],
  ): ExprHListWrapper[V, E1 :: E2 :: HNil, P] =
    new ExprHListWrapper(e1 :: NonEmptyExprHList.tail(e2))

  def wrap[V, E1, E2, E3, P](
    e1: Expr[V, E1, P],
    e2: Expr[V, E2, P],
    e3: Expr[V, E3, P],
  ): ExprHListWrapper[V, E1 :: E2 :: E3 :: HNil, P] =
    new ExprHListWrapper(e1 :: e2 :: NonEmptyExprHList.tail(e3))

  def wrap[V, E1, E2, E3, E4, P](
    e1: Expr[V, E1, P],
    e2: Expr[V, E2, P],
    e3: Expr[V, E3, P],
    e4: Expr[V, E4, P],
  ): ExprHListWrapper[V, E1 :: E2 :: E3 :: E4 :: HNil, P] =
    new ExprHListWrapper(e1 :: e2 :: e3 :: NonEmptyExprHList.tail(e4))

  def wrap[V, E1, E2, E3, E4, E5, P](
    e1: Expr[V, E1, P],
    e2: Expr[V, E2, P],
    e3: Expr[V, E3, P],
    e4: Expr[V, E4, P],
    e5: Expr[V, E5, P],
  ): ExprHListWrapper[V, E1 :: E2 :: E3 :: E4 :: E5 :: HNil, P] =
    new ExprHListWrapper(e1 :: e2 :: e3 :: e4 :: NonEmptyExprHList.tail(e5))

  def wrap[V, E1, E2, E3, E4, E5, E6, P](
    e1: Expr[V, E1, P],
    e2: Expr[V, E2, P],
    e3: Expr[V, E3, P],
    e4: Expr[V, E4, P],
    e5: Expr[V, E5, P],
    e6: Expr[V, E6, P],
  ): ExprHListWrapper[V, E1 :: E2 :: E3 :: E4 :: E5 :: E6 :: HNil, P] =
    new ExprHListWrapper(e1 :: e2 :: e3 :: e4 :: e5 :: NonEmptyExprHList.tail(e6))

  def wrap[V, E1, E2, E3, E4, E5, E6, E7, P](
    e1: Expr[V, E1, P],
    e2: Expr[V, E2, P],
    e3: Expr[V, E3, P],
    e4: Expr[V, E4, P],
    e5: Expr[V, E5, P],
    e6: Expr[V, E6, P],
    e7: Expr[V, E7, P],
  ): ExprHListWrapper[V, E1 :: E2 :: E3 :: E4 :: E5 :: E6 :: E7 :: HNil, P] =
    new ExprHListWrapper(e1 :: e2 :: e3 :: e4 :: e5 :: e6 :: NonEmptyExprHList.tail(e7))

  def wrap[V, E1, E2, E3, E4, E5, E6, E7, E8, P](
    e1: Expr[V, E1, P],
    e2: Expr[V, E2, P],
    e3: Expr[V, E3, P],
    e4: Expr[V, E4, P],
    e5: Expr[V, E5, P],
    e6: Expr[V, E6, P],
    e7: Expr[V, E7, P],
    e8: Expr[V, E8, P],
  ): ExprHListWrapper[V, E1 :: E2 :: E3 :: E4 :: E5 :: E6 :: E7 :: E8 :: HNil, P] =
    new ExprHListWrapper(e1 :: e2 :: e3 :: e4 :: e5 :: e6 :: e7 :: NonEmptyExprHList.tail(e8))

  def wrap[V, E1, E2, E3, E4, E5, E6, E7, E8, E9, P](
    e1: Expr[V, E1, P],
    e2: Expr[V, E2, P],
    e3: Expr[V, E3, P],
    e4: Expr[V, E4, P],
    e5: Expr[V, E5, P],
    e6: Expr[V, E6, P],
    e7: Expr[V, E7, P],
    e8: Expr[V, E8, P],
    e9: Expr[V, E9, P],
  ): ExprHListWrapper[V, E1 :: E2 :: E3 :: E4 :: E5 :: E6 :: E7 :: E8 :: E9 :: HNil, P] =
    new ExprHListWrapper(e1 :: e2 :: e3 :: e4 :: e5 :: e6 :: e7 :: e8 :: NonEmptyExprHList.tail(e9))

  def wrap[V, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, P](
    e1: Expr[V, E1, P],
    e2: Expr[V, E2, P],
    e3: Expr[V, E3, P],
    e4: Expr[V, E4, P],
    e5: Expr[V, E5, P],
    e6: Expr[V, E6, P],
    e7: Expr[V, E7, P],
    e8: Expr[V, E8, P],
    e9: Expr[V, E9, P],
    e10: Expr[V, E10, P],
  ): ExprHListWrapper[V, E1 :: E2 :: E3 :: E4 :: E5 :: E6 :: E7 :: E8 :: E9 :: E10 :: HNil, P] =
    new ExprHListWrapper(e1 :: e2 :: e3 :: e4 :: e5 :: e6 :: e7 :: e8 :: e9 :: NonEmptyExprHList.tail(e10))

  def wrap[V, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, P](
    e1: Expr[V, E1, P],
    e2: Expr[V, E2, P],
    e3: Expr[V, E3, P],
    e4: Expr[V, E4, P],
    e5: Expr[V, E5, P],
    e6: Expr[V, E6, P],
    e7: Expr[V, E7, P],
    e8: Expr[V, E8, P],
    e9: Expr[V, E9, P],
    e10: Expr[V, E10, P],
    e11: Expr[V, E11, P],
  ): ExprHListWrapper[V, E1 :: E2 :: E3 :: E4 :: E5 :: E6 :: E7 :: E8 :: E9 :: E10 :: E11 :: HNil, P] =
    new ExprHListWrapper(e1 :: e2 :: e3 :: e4 :: e5 :: e6 :: e7 :: e8 :: e9 :: e10 :: NonEmptyExprHList.tail(e11))

  def wrap[V, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, P](
    e1: Expr[V, E1, P],
    e2: Expr[V, E2, P],
    e3: Expr[V, E3, P],
    e4: Expr[V, E4, P],
    e5: Expr[V, E5, P],
    e6: Expr[V, E6, P],
    e7: Expr[V, E7, P],
    e8: Expr[V, E8, P],
    e9: Expr[V, E9, P],
    e10: Expr[V, E10, P],
    e11: Expr[V, E11, P],
    e12: Expr[V, E12, P],
  ): ExprHListWrapper[V, E1 :: E2 :: E3 :: E4 :: E5 :: E6 :: E7 :: E8 :: E9 :: E10 :: E11 :: E12 :: HNil, P] =
    new ExprHListWrapper(
      e1 :: e2 :: e3 :: e4 :: e5 :: e6 :: e7 :: e8 :: e9 :: e10 :: e11 :: NonEmptyExprHList.tail(e12),
    )

}

final class ExprHListWrapper[V, L <: HList, P](override protected val exprHList: NonEmptyExprHList[V, Id, L, P])
  extends AnyVal
  with HListOperationWrapper[V, Id, L, P] {

  override type Op[R] = Expr.WrapOutputHList[V, L, R, P]

  override protected def buildOp[R](
    converter: ExprConverter[L, R],
    captureResult: CaptureP[V, Id[R], P],
  ): Expr.WrapOutputHList[V, L, R, P] =
    Expr.WrapOutputHList(exprHList, converter, captureResult)
}

final class GenericIdentity[R] extends Generic[R] {
  override type Repr = R
  override def to(repr: R): Repr = repr
  override def from(repr: Repr): R = repr
}

object GenericIdentity {
  def apply[R]: GenericIdentity[R] = new GenericIdentity[R]
}
