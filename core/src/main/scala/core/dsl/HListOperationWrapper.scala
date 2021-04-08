package com.rallyhealth.vapors.core.dsl

import com.rallyhealth.vapors.core.algebra.{CaptureP, Expr, ExprConverter, NonEmptyExprHList}
import shapeless.ops.hlist.{ToTraversable, Tupler}
import shapeless.{Generic, HList}

trait HListOperationWrapper[V, M[_], L <: HList, P] extends Any {

  type Op[R] <: Expr[V, M[R], P]

  protected def exprHList: NonEmptyExprHList[V, M, L, P]

  protected def buildOp[R](
    converter: ExprConverter[L, R],
    captureResult: CaptureP[V, M[R], P],
  ): Op[R]

  final def asTuple[T](
    implicit
    tupler: Tupler.Aux[L, T],
    captureResult: CaptureP[V, M[T], P],
  ): Op[T] =
    buildOp(
      ExprConverter.asTuple,
      captureResult,
    )

  final def asHList(
    implicit
    captureResult: CaptureP[V, M[L], P],
  ): Op[L] =
    buildOp(
      ExprConverter.asHListIdentity,
      captureResult,
    )

  final def as[R](
    implicit
    gen: Generic.Aux[R, L],
    captureResult: CaptureP[V, M[R], P],
  ): Op[R] =
    buildOp(
      ExprConverter.asProductType,
      captureResult,
    )

}
