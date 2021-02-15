package com.rallyhealth.vapors.core.interpreter

import cats.kernel.Monoid
import cats.{Align, FlatMap, Foldable, Functor, FunctorFilter, Traverse, TraverseFilter}
import com.rallyhealth.vapors.core.algebra.Expr
import com.rallyhealth.vapors.core.data.{ExtractBoolean, FactSet}
import com.rallyhealth.vapors.core.logic.{Conjunction, Disjunction, Negation}
import com.rallyhealth.vapors.core.math.{Addition, Negative, Subtraction}
import shapeless.HList

/**
  * A function interpreter from [[ExprInput]] to any output type constructor that passes all expressions nodes into
  * a single common [[visitGeneric]] function.
  *
  * This class makes it easier to define common behavior when interpreting expressions as functions when you do not
  * need to take into account any specific method signatures or expression node types. It is useful for defining
  * behavior that only relies on the common methods and values of [[Expr]] and [[ExprInput]].
  */
abstract class VisitGenericExprWithProxyFn[F[_] : Foldable, V, P, G[_]]
  extends Expr.Visitor[F, V, P, Lambda[r => ExprInput[F, V] => G[r]]] {

  protected def visitGeneric[M[_] : Foldable, U, R](
    expr: Expr[M, U, R, P],
    input: ExprInput[M, U],
  ): G[R]

  override def visitAddOutputs[R : Addition](expr: Expr.AddOutputs[F, V, R, P]): ExprInput[F, V] => G[R] =
    visitGeneric(expr, _)

  override def visitAnd[R : Conjunction : ExtractBoolean](expr: Expr.And[F, V, R, P]): ExprInput[F, V] => G[R] =
    visitGeneric(expr, _)

  override def visitCollectSomeOutput[M[_] : Foldable, U, R : Monoid](
    expr: Expr.CollectFromOutput[F, V, M, U, R, P],
  ): ExprInput[F, V] => G[R] = visitGeneric(expr, _)

  override def visitConstOutput[R](expr: Expr.ConstOutput[F, V, R, P]): ExprInput[F, V] => G[R] =
    visitGeneric(expr, _)

  override def visitDefine[M[_] : Foldable, T](expr: Expr.Define[M, T, P]): ExprInput[F, V] => G[FactSet] = { input =>
    visitGeneric(expr, input.withValue(input.factTable))
  }

  override def visitEmbed[R](expr: Expr.Embed[F, V, R, P]): ExprInput[F, V] => G[R] =
    visitGeneric(expr, _)

  override def visitExistsInOutput[M[_] : Foldable, U](
    expr: Expr.ExistsInOutput[F, V, M, U, P],
  ): ExprInput[F, V] => G[Boolean] = visitGeneric(expr, _)

  override def visitFilterOutput[M[_] : Foldable : FunctorFilter, R](
    expr: Expr.FilterOutput[F, V, M, R, P],
  ): ExprInput[F, V] => G[M[R]] = visitGeneric(expr, _)

  override def visitFlatMapOutput[M[_] : Foldable : FlatMap, U, X](
    expr: Expr.FlatMapOutput[F, V, M, U, X, P],
  ): ExprInput[F, V] => G[M[X]] = visitGeneric(expr, _)

  override def visitMapOutput[M[_] : Foldable : Functor, U, R](
    expr: Expr.MapOutput[F, V, M, U, R, P],
  ): ExprInput[F, V] => G[M[R]] = visitGeneric(expr, _)

  override def visitNegativeOutput[R : Negative](expr: Expr.NegativeOutput[F, V, R, P]): ExprInput[F, V] => G[R] =
    visitGeneric(expr, _)

  override def visitNot[R : Negation](expr: Expr.Not[F, V, R, P]): ExprInput[F, V] => G[R] =
    visitGeneric(expr, _)

  override def visitOr[R : Disjunction : ExtractBoolean](expr: Expr.Or[F, V, R, P]): ExprInput[F, V] => G[R] =
    visitGeneric(expr, _)

  override def visitOutputIsEmpty[M[_] : Foldable, R](
    expr: Expr.OutputIsEmpty[F, V, M, R, P],
  ): ExprInput[F, V] => G[Boolean] = visitGeneric(expr, _)

  override def visitOutputWithinSet[R](expr: Expr.OutputWithinSet[F, V, R, P]): ExprInput[F, V] => G[Boolean] =
    visitGeneric(expr, _)

  override def visitOutputWithinWindow[R](expr: Expr.OutputWithinWindow[F, V, R, P]): ExprInput[F, V] => G[Boolean] =
    visitGeneric(expr, _)

  override def visitReturnInput(expr: Expr.ReturnInput[F, V, P]): ExprInput[F, V] => G[F[V]] =
    visitGeneric(expr, _)

  override def visitSelectFromOutput[S, R](expr: Expr.SelectFromOutput[F, V, S, R, P]): ExprInput[F, V] => G[R] =
    visitGeneric(expr, _)

  override def visitSortOutput[M[_], R](expr: Expr.SortOutput[F, V, M, R, P]): ExprInput[F, V] => G[M[R]] =
    visitGeneric(expr, _)

  override def visitSubtractOutputs[R : Subtraction](expr: Expr.SubtractOutputs[F, V, R, P]): ExprInput[F, V] => G[R] =
    visitGeneric(expr, _)

  override def visitTakeFromOutput[M[_] : Traverse : TraverseFilter, R](
    expr: Expr.TakeFromOutput[F, V, M, R, P],
  ): ExprInput[F, V] => G[M[R]] = visitGeneric(expr, _)

  override def visitUsingDefinitions[R](expr: Expr.UsingDefinitions[F, V, R, P]): ExprInput[F, V] => G[R] =
    visitGeneric(expr, _)

  override def visitWhen[R](expr: Expr.When[F, V, R, P]): ExprInput[F, V] => G[R] =
    visitGeneric(expr, _)

  override def visitWithFactsOfType[T, R](expr: Expr.WithFactsOfType[T, R, P]): ExprInput[F, V] => G[R] = { input =>
    visitGeneric(expr, input.withValue(input.factTable))
  }

  override def visitWrapOutput[T <: HList, R](expr: Expr.WrapOutput[F, V, T, R, P]): ExprInput[F, V] => G[R] =
    visitGeneric(expr, _)

  override def visitZipOutput[M[_] : Align : FunctorFilter, L <: HList, R](
    expr: Expr.ZipOutput[F, V, M, L, R, P],
  ): ExprInput[F, V] => G[M[R]] = visitGeneric(expr, _)
}
