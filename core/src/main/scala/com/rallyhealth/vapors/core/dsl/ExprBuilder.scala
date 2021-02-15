package com.rallyhealth.vapors.core.dsl

import cats.{FlatMap, Foldable, Functor, FunctorFilter, Id, Order, Traverse, TraverseFilter}
import com.rallyhealth.vapors.core.algebra.{CaptureP, Expr, ExprSorter}
import com.rallyhealth.vapors.core.data.{Evidence, TypedFact, Window}
import com.rallyhealth.vapors.core.lens.NamedLens
import com.rallyhealth.vapors.core.math.{Addition, Negative, Subtraction}

import scala.collection.Factory
import scala.reflect.runtime.universe.TypeTag

sealed class ExprBuilder[F[_], V, M[_], U, P](val returnOutput: Expr[F, V, M[U], P]) {

  type CaptureResult[R] = CaptureP[F, V, R, P]

  type CaptureInput[G[_]] = CaptureP[G, V, G[V], P]
  type CaptureAllInputCond = CaptureResult[Boolean]
  type CaptureAllInput = CaptureInput[F]
  type CaptureEachInput = CaptureInput[Id]

  type CaptureEachOutputResult[R] = CaptureP[Id, U, R, P]
  type CaptureEachOutput = CaptureEachOutputResult[U]
  type CaptureEachOutputCond = CaptureEachOutputResult[Boolean]

  def returnInput(implicit captureInput: CaptureAllInput): Expr[F, V, F[V], P] = Expr.ReturnInput(captureInput)

  def embed[R](expr: Expr[M, U, R, P]): ExprBuilder[M, U, Id, R, P] = new ExprBuilder[M, U, Id, R, P](expr)
}

object ExprBuilder {

  type ValId[V, P] = ValExprBuilder[V, V, P]
  type ValFn[V, R, P] = ValExprBuilder[V, V, P] => ExprBuilder[Id, V, Id, R, P]

  type FoldableId[F[_], V, P] = FoldableExprBuilder[F, V, F, V, P]
  type FoldableFn[F[_], V, M[_], U, P] = FoldableId[F, V, P] => ExprBuilder[F, V, M, U, P]
}

trait ExprBuilderSyntax {

  /**
    * Implicitly allows embedding any expression that only requires the FactTable into expression.
    *
    * @see [[Expr.Embed]]
    */
  implicit def embedExpr[F[_], V, R, P](
    expr: RootExpr[R, P],
  )(implicit
    captureResult: CaptureP[F, V, R, P],
  ): Expr.Embed[F, V, R, P] = Expr.Embed(expr, captureResult)

  implicit def liftValExpr[V, R, P](expr: Expr[Id, V, R, P]): ValExprBuilder[V, R, P] =
    new ValExprBuilder(expr)

  implicit def returnFoldableExprOutput[F[_], V, M[_], U, P](
    builder: FoldableExprBuilder[F, V, M, U, P],
  ): Expr[F, V, M[U], P] =
    builder.returnOutput

  implicit def returnValExprOutput[V, R, P](builder: ValExprBuilder[V, R, P]): Expr[Id, V, R, P] =
    builder.returnOutput

  implicit def addTo[R : Addition](lhs: R): AdditionBuilderOps[R] = new AdditionBuilderOps(lhs)

  implicit def subtractFrom[R : Subtraction](lhs: R): SubtractBuilderOps[R] = new SubtractBuilderOps(lhs)
}

final class AdditionBuilderOps[R : Addition](number: R) {

  def +[V, P](
    builder: ValExprBuilder[V, R, P],
  )(implicit
    captureResult: builder.CaptureResult[R],
  ): ValExprBuilder[V, R, P] = {
    builder.addTo(number)
  }
}

final class SubtractBuilderOps[R : Subtraction](number: R) {

  def -[V, P](
    builder: ValExprBuilder[V, R, P],
  )(implicit
    captureResult: builder.CaptureResult[R],
  ): ValExprBuilder[V, R, P] = {
    builder.subtractFrom(number)
  }
}

sealed class FoldableExprBuilder[F[_] : Foldable, V, M[_] : Foldable, U, P](returnOutput: Expr[F, V, M[U], P])
  extends ExprBuilder[F, V, M, U, P](returnOutput) {

  def toList(
    implicit
    ev: M[U] <:< Iterable[U],
    captureOutput: CaptureP[F, V, List[U], P],
  ): FoldableExprBuilder[F, V, List, U, P] =
    to(List)

  def toSet(
    implicit
    ev: M[U] <:< Iterable[U],
    captureOutput: CaptureP[F, V, Set[U], P],
  ): FoldableExprBuilder[F, V, Set, U, P] = {
    to(Set)
  }

  def to[N[_] : Foldable](
    factory: Factory[U, N[U]],
  )(implicit
    ev: M[U] <:< Iterable[U],
    captureOutput: CaptureP[F, V, N[U], P],
  ): FoldableExprBuilder[F, V, N, U, P] =
    new FoldableExprBuilder({
      Expr.SelectFromOutput[F, V, M[U], N[U], P](
        returnOutput,
        NamedLens.id[M[U]].asIterable.to(factory),
        captureOutput,
      )
    })

  def sorted(
    implicit
    orderU: Order[U],
    ev: M[U] <:< Seq[U],
    tt: TypeTag[U],
    factory: Factory[U, M[U]],
    captureResult: CaptureResult[M[U]],
  ): FoldableExprBuilder[F, V, M, U, P] =
    new FoldableExprBuilder(Expr.SortOutput(returnOutput, ExprSorter.byNaturalOrder[M, U], captureResult))

  def sortBy[R : Order](
    buildLens: NamedLens.Fn[U, R],
  )(implicit
    ev: M[U] <:< Seq[U],
    tt: TypeTag[U],
    factory: Factory[U, M[U]],
    captureResult: CaptureResult[M[U]],
  ): FoldableExprBuilder[F, V, M, U, P] = {
    val lens = buildLens(NamedLens.id[U])
    new FoldableExprBuilder(Expr.SortOutput(returnOutput, ExprSorter.byField[M, U, R](lens), captureResult))
  }

  def take(
    n: Int,
  )(implicit
    traverseM: Traverse[M],
    traverseFilterM: TraverseFilter[M],
    captureAllOutput: CaptureP[F, V, M[U], P],
  ): FoldableExprBuilder[F, V, M, U, P] =
    new FoldableExprBuilder(Expr.TakeFromOutput(returnOutput, n, captureAllOutput))

  def headOption(
    implicit
    traverseM: Traverse[M],
    traverseFilterM: TraverseFilter[M],
    ev: M[U] <:< Iterable[U],
    captureAllOutput: CaptureP[F, V, M[U], P],
    captureHeadOutput: CaptureP[F, V, Option[U], P],
  ): FoldableExprBuilder[F, V, Option, U, P] =
    new FoldableExprBuilder(
      Expr.SelectFromOutput(take(1), NamedLens.id[M[U]].headOption, captureHeadOutput),
    )

  def map[R](
    buildFn: ValExprBuilder[U, U, P] => ExprBuilder[Id, U, Id, R, P],
  )(implicit
    functorM: Functor[M],
    postEachOutput: CaptureP[Id, U, U, P],
    postMap: CaptureP[F, V, M[R], P],
  ): FoldableExprBuilder[F, V, M, R, P] = {
    val mapExpr = buildFn(
      new ValExprBuilder(Expr.ReturnInput[Id, U, P](postEachOutput)),
    )
    val next = Expr.MapOutput(returnOutput, mapExpr.returnOutput, postMap)
    new FoldableExprBuilder(next)
  }

  def flatMap[X](
    buildFn: ValExprBuilder[U, U, P] => ExprBuilder[Id, U, M, X, P],
  )(implicit
    flatMapM: FlatMap[M],
    postEachOutput: CaptureP[Id, U, U, P],
    postFlatMap: CaptureP[F, V, M[X], P],
  ): FoldableExprBuilder[F, V, M, X, P] = {
    val flatMapExpr = buildFn(
      new ValExprBuilder(Expr.ReturnInput[Id, U, P](postEachOutput)),
    )
    val next = Expr.FlatMapOutput(returnOutput, flatMapExpr.returnOutput, postFlatMap)
    new FoldableExprBuilder(next)
  }

  def isEmpty(
    implicit
    captureResult: CaptureAllInputCond,
  ): FoldInExprBuilder[F, V, Boolean, P] =
    new FoldInExprBuilder(Expr.OutputIsEmpty(returnOutput, captureResult))

  def exists(
    buildFn: ValExprBuilder[U, U, P] => ExprBuilder[Id, U, Id, Boolean, P],
  )(implicit
    postEachOutput: CaptureP[Id, U, U, P],
    captureResult: CaptureAllInputCond,
  ): FoldInExprBuilder[F, V, Boolean, P] = {
    val condExpr = buildFn(new ValExprBuilder(Expr.ReturnInput[Id, U, P](postEachOutput)))
    val next = Expr.ExistsInOutput(returnOutput, condExpr.returnOutput, captureResult)
    new FoldInExprBuilder(next)
  }

  def filter(
    buildFn: ValExprBuilder[U, U, P] => ExprBuilder[Id, U, Id, Boolean, P],
  )(implicit
    filterM: FunctorFilter[M],
    captureEachOutput: CaptureEachOutput,
    captureResult: CaptureResult[M[U]],
  ): FoldableExprBuilder[F, V, M, U, P] = {
    val condExpr = buildFn(
      new ValExprBuilder(Expr.ReturnInput[Id, U, P](captureEachOutput)),
    )
    new FoldableExprBuilder(Expr.FilterOutput(returnOutput, condExpr.returnOutput, captureResult))
  }

  def containsAny(
    validValues: Set[U],
  )(implicit
    filterM: FunctorFilter[M],
    captureEachOutput: CaptureEachOutput,
    captureEachOutputCond: CaptureEachOutputCond,
    captureResult: CaptureResult[M[U]],
    captureCond: CaptureAllInputCond,
  ): FoldInExprBuilder[F, V, Boolean, P] =
    new FoldInExprBuilder(
      Expr.Not(
        filter(_ in validValues).isEmpty,
        captureCond,
      ),
    )
}

final class FoldInExprBuilder[F[_] : Foldable, V, R, P](returnOutput: Expr[F, V, R, P])
  extends FoldableExprBuilder[F, V, Id, R, P](returnOutput)

final class FoldOutExprBuilder[V, M[_] : Foldable, U, P](returnOutput: Expr[Id, V, M[U], P])
  extends FoldableExprBuilder[Id, V, M, U, P](returnOutput)

// TODO: Combine with FoldIn / FoldOut?
final class ValExprBuilder[V, R, P](returnOutput: Expr[Id, V, R, P])
  extends ExprBuilder[Id, V, Id, R, P](returnOutput) {

  @inline private def buildGetExpr[N[_], X](
    buildLens: NamedLens.Fn[R, N[X]],
  )(implicit
    captureResult: CaptureP[Id, V, N[X], P],
  ): Expr[Id, V, N[X], P] = {
    val lens = buildLens(NamedLens.id[R])
    // if the previous node was a SelectFromOutput, then combine the lenses and produce a single node
    returnOutput match {
      case prev: Expr.SelectFromOutput[Id, V, s, R, P] =>
        // capture the starting type as an existential type parameter 's'
        // it is ignored in the return type after the compile proves that this code is safe
        Expr.SelectFromOutput[Id, V, s, N[X], P](prev.inputExpr, prev.lens.andThen(lens), captureResult)
      case _ =>
        // otherwise, build the lens as a new SelectFromOutput node
        Expr.SelectFromOutput(returnOutput, lens, captureResult)
    }
  }

  def get[X](
    buildLens: NamedLens.Fn[R, X],
  )(implicit
    captureResult: CaptureP[Id, V, X, P],
  ): ValExprBuilder[V, X, P] =
    new ValExprBuilder(buildGetExpr[Id, X](buildLens))

  def getFoldable[N[_] : Foldable, X](
    buildLens: NamedLens.Fn[R, N[X]],
  )(implicit
    captureResult: CaptureP[Id, V, N[X], P],
  ): FoldOutExprBuilder[V, N, X, P] =
    new FoldOutExprBuilder(buildGetExpr(buildLens))

  def value[X](
    implicit
    ev: R <:< TypedFact[X],
    captureResult: CaptureResult[X],
  ): ValExprBuilder[V, X, P] =
    new ValExprBuilder(buildGetExpr[Id, X](_.field("value", _.value)))

  def in(accepted: Set[R])(implicit captureOutput: CaptureResult[Boolean]): ValExprBuilder[V, Boolean, P] =
    new ValExprBuilder(Expr.OutputWithinSet(returnOutput, accepted, captureOutput))

  def add(
    rhs: R,
  )(implicit
    R: Addition[R],
    captureResult: CaptureResult[R],
  ): ValExprBuilder[V, R, P] =
    this + rhs

  def +(
    rhs: R,
  )(implicit
    R: Addition[R],
    captureResult: CaptureResult[R],
  ): ValExprBuilder[V, R, P] =
    new ValExprBuilder(ExprDsl.add(returnOutput, Expr.ConstOutput(rhs, Evidence.none, captureResult)))

  def addTo(
    lhs: R,
  )(implicit
    R: Addition[R],
    captureResult: CaptureResult[R],
  ): ValExprBuilder[V, R, P] =
    new ValExprBuilder(ExprDsl.add(Expr.ConstOutput(lhs, Evidence.none, captureResult), returnOutput))

  def subtract(
    rhs: R,
  )(implicit
    R: Subtraction[R],
    captureResult: CaptureResult[R],
  ): ValExprBuilder[V, R, P] =
    this - rhs

  def -(
    rhs: R,
  )(implicit
    R: Subtraction[R],
    captureResult: CaptureResult[R],
  ): ValExprBuilder[V, R, P] =
    new ValExprBuilder(ExprDsl.subtract(returnOutput, Expr.ConstOutput(rhs, Evidence.none, captureResult)))

  def subtractFrom(
    lhs: R,
  )(implicit
    R: Subtraction[R],
    captureResult: CaptureResult[R],
  ): ValExprBuilder[V, R, P] =
    new ValExprBuilder(ExprDsl.subtract(Expr.ConstOutput(lhs, Evidence.none, captureResult), returnOutput))

  def unary_-(
    implicit
    R: Negative[R],
    captureResult: CaptureResult[R],
  ): ValExprBuilder[V, R, P] =
    new ValExprBuilder(ExprDsl.negative(returnOutput))

  def within(
    window: Window[R],
  )(implicit
    captureResult: CaptureAllInputCond,
  ): ValExprBuilder[V, Boolean, P] =
    new ValExprBuilder(ExprDsl.within(returnOutput, window))

  def isEqualTo(
    value: R,
  )(implicit
    orderR: Order[R],
    captureResult: CaptureAllInputCond,
  ): ValExprBuilder[V, Boolean, P] =
    within(Window.equalTo(value))

  def ===(
    value: R,
  )(implicit
    orderR: Order[R],
    captureResult: CaptureAllInputCond,
  ): ValExprBuilder[V, Boolean, P] =
    within(Window.equalTo(value))

  def !==(
    value: R,
  )(implicit
    orderR: Order[R],
    captureResult: CaptureAllInputCond,
  ): ValExprBuilder[V, Boolean, P] =
    Expr.Not(within(Window.equalTo(value)), captureResult)

  def <(
    value: R,
  )(implicit
    orderR: Order[R],
    captureResult: CaptureAllInputCond,
  ): ValExprBuilder[V, Boolean, P] =
    within(Window.lessThan(value))

  def <=(
    value: R,
  )(implicit
    orderR: Order[R],
    captureResult: CaptureAllInputCond,
  ): ValExprBuilder[V, Boolean, P] =
    within(Window.lessThanOrEqual(value))

  def >(
    value: R,
  )(implicit
    orderR: Order[R],
    captureResult: CaptureAllInputCond,
  ): ValExprBuilder[V, Boolean, P] =
    within(Window.greaterThan(value))

  def >=(
    value: R,
  )(implicit
    orderR: Order[R],
    captureResult: CaptureAllInputCond,
  ): ValExprBuilder[V, Boolean, P] =
    within(Window.greaterThanOrEqual(value))
}
