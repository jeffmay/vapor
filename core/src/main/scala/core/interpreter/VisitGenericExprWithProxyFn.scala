package com.rallyhealth.vapors

package core.interpreter

import core.algebra.Expr
import core.data.{ExtractBoolean, FactSet}
import core.logic.{Conjunction, Disjunction, Negation}
import core.math._

import cats._
import cats.kernel.Monoid
import shapeless.HList

import scala.collection.MapView

/**
  * A function interpreter from [[ExprInput]] to any output type constructor that passes all expressions nodes into
  * a single common [[visitGeneric]] function.
  *
  * This class makes it easier to define common behavior when interpreting expressions as functions when you do not
  * need to take into account any specific method signatures or expression node types. It is useful for defining
  * behavior that only relies on the common methods and values of [[Expr]] and [[ExprInput]].
  */
abstract class VisitGenericExprWithProxyFn[V, P, G[_]] extends Expr.Visitor[V, P, Lambda[r => ExprInput[V] => G[r]]] {

  protected def visitGeneric[U, R](
    expr: Expr[U, R, P],
    input: ExprInput[U],
  ): G[R]

  override def visitAddOutputs[R : Addition](expr: Expr.AddOutputs[V, R, P]): ExprInput[V] => G[R] =
    visitGeneric(expr, _)

  override def visitAnd[R : Conjunction : ExtractBoolean](expr: Expr.And[V, R, P]): ExprInput[V] => G[R] =
    visitGeneric(expr, _)

  override def visitCollectSomeOutput[M[_] : Foldable, U, R : Monoid](
    expr: Expr.CollectFromOutput[V, M, U, R, P],
  ): ExprInput[V] => G[R] = visitGeneric(expr, _)

  override def visitConcatOutput[M[_] : MonoidK, R](expr: Expr.ConcatOutput[V, M, R, P]): ExprInput[V] => G[M[R]] =
    visitGeneric(expr, _)

  override def visitConstOutput[R](expr: Expr.ConstOutput[V, R, P]): ExprInput[V] => G[R] =
    visitGeneric(expr, _)

  override def visitCustomFunction[A, R](expr: Expr.CustomFunction[V, A, R, P]): ExprInput[V] => G[R] =
    visitGeneric(expr, _)

  override def visitDefine[M[_] : Foldable, T](expr: Expr.Define[M, T, P]): ExprInput[V] => G[FactSet] = { input =>
    visitGeneric(expr, input.withValue(input.factTable))
  }

  override def visitDivideOutputs[R : Division](expr: Expr.DivideOutputs[V, R, P]): ExprInput[V] => G[R] =
    visitGeneric(expr, _)

  override def visitEmbed[R](expr: Expr.Embed[V, R, P]): ExprInput[V] => G[R] =
    visitGeneric(expr, _)

  override def visitExistsInOutput[M[_] : Foldable, U](
    expr: Expr.ExistsInOutput[V, M, U, P],
  ): ExprInput[V] => G[Boolean] = visitGeneric(expr, _)

  override def visitFilterOutput[M[_] : Foldable : FunctorFilter, R](
    expr: Expr.FilterOutput[V, M, R, P],
  ): ExprInput[V] => G[M[R]] = visitGeneric(expr, _)

  override def visitFlatMapOutput[M[_] : Foldable : FlatMap, U, X](
    expr: Expr.FlatMapOutput[V, M, U, X, P],
  ): ExprInput[V] => G[M[X]] = visitGeneric(expr, _)

  override def visitGroupOutput[M[_] : Foldable, U : Order, K](
    expr: Expr.GroupOutput[V, M, U, K, P],
  ): ExprInput[V] => G[MapView[K, Seq[U]]] = visitGeneric(expr, _)

  override def visitMapOutput[M[_] : Foldable : Functor, U, R](
    expr: Expr.MapOutput[V, M, U, R, P],
  ): ExprInput[V] => G[M[R]] = visitGeneric(expr, _)

  override def visitMultiplyOutputs[R : Multiplication](expr: Expr.MultiplyOutputs[V, R, P]): ExprInput[V] => G[R] =
    visitGeneric(expr, _)

  override def visitNegativeOutput[R : Negative](expr: Expr.NegativeOutput[V, R, P]): ExprInput[V] => G[R] =
    visitGeneric(expr, _)

  override def visitNot[R : Negation](expr: Expr.Not[V, R, P]): ExprInput[V] => G[R] =
    visitGeneric(expr, _)

  override def visitOr[R : Disjunction : ExtractBoolean](expr: Expr.Or[V, R, P]): ExprInput[V] => G[R] =
    visitGeneric(expr, _)

  override def visitOutputIsEmpty[M[_] : Foldable, R](
    expr: Expr.OutputIsEmpty[V, M, R, P],
  ): ExprInput[V] => G[Boolean] = visitGeneric(expr, _)

  override def visitOutputWithinSet[R](expr: Expr.OutputWithinSet[V, R, P]): ExprInput[V] => G[Boolean] =
    visitGeneric(expr, _)

  override def visitOutputWithinWindow[R](expr: Expr.OutputWithinWindow[V, R, P]): ExprInput[V] => G[Boolean] =
    visitGeneric(expr, _)

  override def visitFoldOutput[M[_] : Foldable, R : Monoid](expr: Expr.FoldOutput[V, M, R, P]): ExprInput[V] => G[R] =
    visitGeneric(expr, _)

  override def visitReturnInput(expr: Expr.ReturnInput[V, P]): ExprInput[V] => G[V] =
    visitGeneric(expr, _)

  override def visitSelectFromOutput[S, R](expr: Expr.SelectFromOutput[V, S, R, P]): ExprInput[V] => G[R] =
    visitGeneric(expr, _)

  override def visitSortOutput[M[_], R](expr: Expr.SortOutput[V, M, R, P]): ExprInput[V] => G[M[R]] =
    visitGeneric(expr, _)

  override def visitSubtractOutputs[R : Subtraction](expr: Expr.SubtractOutputs[V, R, P]): ExprInput[V] => G[R] =
    visitGeneric(expr, _)

  override def visitTakeFromOutput[M[_] : Traverse : TraverseFilter, R](
    expr: Expr.TakeFromOutput[V, M, R, P],
  ): ExprInput[V] => G[M[R]] = visitGeneric(expr, _)

  override def visitWrapOutputSeq[R](expr: Expr.WrapOutputSeq[V, R, P]): ExprInput[V] => G[Seq[R]] =
    visitGeneric(expr, _)

  override def visitUsingDefinitions[R](expr: Expr.UsingDefinitions[V, R, P]): ExprInput[V] => G[R] =
    visitGeneric(expr, _)

  override def visitWhen[R](expr: Expr.When[V, R, P]): ExprInput[V] => G[R] =
    visitGeneric(expr, _)

  override def visitWithFactsOfType[T, R](expr: Expr.WithFactsOfType[T, R, P]): ExprInput[V] => G[R] = { input =>
    visitGeneric(expr, input.withValue(input.factTable))
  }

  override def visitWrapOutputHList[T <: HList, R](expr: Expr.WrapOutputHList[V, T, R, P]): ExprInput[V] => G[R] =
    visitGeneric(expr, _)

  override def visitZipOutput[M[_] : Align : FunctorFilter, L <: HList, R](
    expr: Expr.ZipOutput[V, M, L, R, P],
  ): ExprInput[V] => G[M[R]] = visitGeneric(expr, _)
}
