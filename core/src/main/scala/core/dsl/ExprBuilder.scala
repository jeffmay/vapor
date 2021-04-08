package com.rallyhealth.vapors

package core.dsl

import core.algebra.{CaptureP, Expr, ExprSorter}
import core.data.{Evidence, TypedFact, Window}
import core.lens.NamedLens
import core.math._

import cats._

import scala.collection.{Factory, MapView, View}
import scala.reflect.runtime.universe.TypeTag

// TODO: Make this more reusable for MapViewExprBuilder
sealed class ExprBuilder[V, M[_], U, P](val returnOutput: Expr[V, M[U], P]) {

  type CaptureResult[R] = CaptureP[V, R, P]

  type CaptureCondResult = CaptureResult[Boolean]

  type CaptureInputResult[R] = CaptureP[U, R, P]
  type CaptureInputAsResult = CaptureInputResult[U]
  type CaptureInputCondResult = CaptureInputResult[Boolean]

  def returnInput(implicit captureResult: CaptureResult[V]): Expr[V, V, P] = Expr.ReturnInput(captureResult)

  /**
    * Embed the result of an expression with the input of the current builder.
    */
  def embedResult[A](
    expr: RootExpr[A, P],
  )(implicit
    captureEmbed: CaptureP[V, A, P],
  ): ValExprBuilder[V, A, P] =
    new ValExprBuilder[V, A, P](Expr.Embed(expr, captureEmbed))

  /**
    * Embed a constant value with the input of the current builder.
    */
  def embedConst[A](
    value: A,
  )(implicit
    captureConst: CaptureRootExpr[A, P],
    captureEmbed: CaptureP[V, A, P],
  ): ValExprBuilder[V, A, P] =
    new ValExprBuilder[V, A, P](Expr.Embed(Expr.ConstOutput(value, Evidence.none, captureConst), captureEmbed))
}

object ExprBuilder {

  type ValId[V, P] = ValExprBuilder[V, V, P]
  type ValFn[V, R, P] = ValExprBuilder[V, V, P] => ExprBuilder[V, Id, R, P]

  type FoldableId[F[_], V, P] = FoldableExprBuilder[F[V], F, V, P]
  type FoldableFn[F[_], V, M[_], U, P] = FoldableId[F, V, P] => ExprBuilder[F[V], M, U, P]
}

trait ExprBuilderSyntax {

  /**
    * Implicitly allows embedding any expression that only requires the FactTable into expression.
    *
    * @see [[Expr.Embed]]
    */
  implicit def embedExpr[V, R, P](
    expr: RootExpr[R, P],
  )(implicit
    captureResult: CaptureP[V, R, P],
  ): Expr.Embed[V, R, P] = Expr.Embed(expr, captureResult)

  implicit def liftValExpr[V, R, P](expr: Expr[V, R, P]): ValExprBuilder[V, R, P] =
    new ValExprBuilder(expr)

  implicit def returnFoldableExprOutput[V, M[_], U, P](builder: FoldableExprBuilder[V, M, U, P]): Expr[V, M[U], P] =
    builder.returnOutput

  implicit def returnValExprOutput[V, R, P](builder: ValExprBuilder[V, R, P]): Expr[V, R, P] =
    builder.returnOutput

  implicit def addTo[R : Addition](lhs: R): AdditionBuilderOps[R] = new AdditionBuilderOps(lhs)

  implicit def subtractFrom[R : Subtraction](lhs: R): SubtractBuilderOps[R] = new SubtractBuilderOps(lhs)

  implicit def divideFrom[R : Division](lhs: R): DivisionBuilderOps[R] = new DivisionBuilderOps(lhs)

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

final class MultiplicationBuilderOps[R : Multiplication](number: R) {

  def *[V, P](
    builder: ValExprBuilder[V, R, P],
  )(implicit
    captureResult: builder.CaptureResult[R],
  ): ValExprBuilder[V, R, P] = {
    builder.multiplyTo(number)
  }
}

final class DivisionBuilderOps[R : Division](number: R) {

  def /[V, P](
    builder: ValExprBuilder[V, R, P],
  )(implicit
    captureResult: builder.CaptureResult[R],
  ): ValExprBuilder[V, R, P] = {
    builder.divideFrom(number)
  }
}

// TODO: Rename? Is this always foldable?
final class FoldableExprBuilder[V, M[_], U, P](returnOutput: Expr[V, M[U], P])
  extends ExprBuilder[V, M, U, P](returnOutput) {

  def toList(
    implicit
    ev: M[U] <:< IterableOnce[U],
    captureResult: CaptureResult[List[U]],
  ): FoldableExprBuilder[V, List, U, P] =
    to(List)

  def toSet(
    implicit
    ev: M[U] <:< IterableOnce[U],
    captureResult: CaptureResult[Set[U]],
  ): FoldableExprBuilder[V, Set, U, P] =
    to(Set)

  def toMap[K, X](
    implicit
    ev: M[U] <:< IterableOnce[(K, X)],
    captureResult: CaptureResult[MapView[K, X]],
  ): MapViewExprBuilder[V, K, X, P] =
    new MapViewExprBuilder(
      Expr.SelectFromOutput[V, M[U], MapView[K, X], P](
        returnOutput,
        NamedLens.id[M[U]].toMapView,
        captureResult,
      ),
    )

  def to[N[_] : Foldable](
    factory: Factory[U, N[U]],
  )(implicit
    ev: M[U] <:< IterableOnce[U],
    captureResult: CaptureResult[N[U]],
  ): FoldableExprBuilder[V, N, U, P] =
    new FoldableExprBuilder(
      Expr.SelectFromOutput[V, M[U], N[U], P](
        returnOutput,
        NamedLens.id[M[U]].asIterable.to(factory),
        captureResult,
      ),
    )

  def sorted(
    implicit
    orderU: Order[U],
    ev: M[U] <:< Seq[U],
    tt: TypeTag[U],
    factory: Factory[U, M[U]],
    captureResult: CaptureResult[M[U]],
  ): FoldableExprBuilder[V, M, U, P] =
    new FoldableExprBuilder(Expr.SortOutput(returnOutput, ExprSorter.byNaturalOrder[M, U], captureResult))

  def sortBy[R : Order](
    buildLens: NamedLens.Fn[U, R],
  )(implicit
    ev: M[U] <:< Seq[U],
    tt: TypeTag[U],
    factory: Factory[U, M[U]],
    captureResult: CaptureResult[M[U]],
  ): FoldableExprBuilder[V, M, U, P] = {
    val lens = buildLens(NamedLens.id[U])
    new FoldableExprBuilder(Expr.SortOutput(returnOutput, ExprSorter.byField[M, U, R](lens), captureResult))
  }

  def groupBy[K](
    buildKeyLens: NamedLens.Fn[U, K],
  )(implicit
    foldableM: Foldable[M],
    orderU: Order[U],
    captureSelect: CaptureResult[View[(K, Seq[U])]],
    captureResult: CaptureResult[MapView[K, Seq[U]]],
  ): MapViewExprBuilder[V, K, Seq[U], P] = {
    val keyLens = buildKeyLens(NamedLens.id[U])
    new MapViewExprBuilder(Expr.GroupOutput(returnOutput, keyLens, captureResult))
  }

  def take(
    n: Int,
  )(implicit
    traverseM: Traverse[M],
    traverseFilterM: TraverseFilter[M],
    captureResult: CaptureResult[M[U]],
  ): FoldableExprBuilder[V, M, U, P] =
    new FoldableExprBuilder(Expr.TakeFromOutput(returnOutput, n, captureResult))

  def headOption(
    implicit
    traverseM: Traverse[M],
    traverseFilterM: TraverseFilter[M],
    ev: M[U] <:< Iterable[U],
    captureAllResults: CaptureResult[M[U]],
    captureHeadResult: CaptureResult[Option[U]],
  ): FoldableExprBuilder[V, Option, U, P] =
    new FoldableExprBuilder(
      Expr.SelectFromOutput(take(1), NamedLens.id[M[U]].headOption, captureHeadResult),
    )

  def map[R](
    buildFn: ValExprBuilder[U, U, P] => ExprBuilder[U, Id, R, P],
  )(implicit
    foldableM: Foldable[M],
    functorM: Functor[M],
    postEachOutput: CaptureInputAsResult,
    postMap: CaptureResult[M[R]],
  ): FoldableExprBuilder[V, M, R, P] = {
    val mapExpr = buildFn(
      new ValExprBuilder(Expr.ReturnInput(postEachOutput)),
    )
    val next = Expr.MapOutput(returnOutput, mapExpr.returnOutput, postMap)
    new FoldableExprBuilder(next)
  }

  def flatMap[X](
    buildFn: ValExprBuilder[U, U, P] => ExprBuilder[U, M, X, P],
  )(implicit
    foldableM: Foldable[M],
    flatMapM: FlatMap[M],
    postEachInput: CaptureInputAsResult,
    postFlatMap: CaptureResult[M[X]],
  ): FoldableExprBuilder[V, M, X, P] = {
    val flatMapExpr = buildFn(
      new ValExprBuilder(Expr.ReturnInput(postEachInput)),
    )
    val next = Expr.FlatMapOutput(returnOutput, flatMapExpr.returnOutput, postFlatMap)
    new FoldableExprBuilder(next)
  }

  def fold(
    implicit
    foldableM: Foldable[M],
    monoidU: Monoid[U],
    captureResult: CaptureResult[U],
  ): ValExprBuilder[V, U, P] = {
    new ValExprBuilder(Expr.FoldOutput(returnOutput, captureResult))
  }

  def isEmpty(
    implicit
    foldableM: Foldable[M],
    captureResult: CaptureCondResult,
  ): ValExprBuilder[V, Boolean, P] =
    new ValExprBuilder(Expr.OutputIsEmpty(returnOutput, captureResult))

  def exists(
    buildFn: ValExprBuilder[U, U, P] => ExprBuilder[U, Id, Boolean, P],
  )(implicit
    foldableM: Foldable[M],
    postEachOutput: CaptureP[U, U, P],
    captureResult: CaptureCondResult,
  ): ValExprBuilder[V, Boolean, P] = {
    val condExpr = buildFn(new ValExprBuilder(Expr.ReturnInput(postEachOutput)))
    val next = Expr.ExistsInOutput(returnOutput, condExpr.returnOutput, captureResult)
    new ValExprBuilder(next)
  }

  def filter(
    buildFn: ValExprBuilder[U, U, P] => ExprBuilder[U, Id, Boolean, P],
  )(implicit
    foldableM: Foldable[M],
    filterM: FunctorFilter[M],
    captureInput: CaptureInputResult[U],
    captureResult: CaptureResult[M[U]],
  ): FoldableExprBuilder[V, M, U, P] = {
    val condExpr = buildFn(
      new ValExprBuilder(Expr.ReturnInput(captureInput)),
    )
    new FoldableExprBuilder(Expr.FilterOutput(returnOutput, condExpr.returnOutput, captureResult))
  }

  def containsAny(
    validValues: Set[U],
  )(implicit
    foldableM: Foldable[M],
    filterM: FunctorFilter[M],
    captureCond: CaptureCondResult,
    captureFilterCond: CaptureInputCondResult,
    captureFilterInput: CaptureInputResult[U],
    captureFilterResult: CaptureResult[M[U]],
  ): ValExprBuilder[V, Boolean, P] =
    new ValExprBuilder[V, Boolean, P](
      Expr.Not(
        filter(_ in validValues).isEmpty,
        captureCond,
      ),
    )
}

// TODO: Share any methods with FoldableExprBuilder?
final class ValExprBuilder[V, R, P](override val returnOutput: Expr[V, R, P])
  extends ExprBuilder[V, Id, R, P](returnOutput) {

  @inline private def buildGetExpr[N[_], X](
    buildLens: NamedLens.Fn[R, N[X]],
  )(implicit
    captureResult: CaptureResult[N[X]],
  ): Expr[V, N[X], P] = {
    val lens = buildLens(NamedLens.id[R])
    // if the previous node was a SelectFromOutput, then combine the lenses and produce a single node
    returnOutput match {
      case prev: Expr.SelectFromOutput[V, s, R, P] =>
        // capture the starting type as an existential type parameter 's'
        // it is ignored in the return type after the compile proves that this code is safe
        Expr.SelectFromOutput[V, s, N[X], P](prev.inputExpr, prev.lens.andThen(lens), captureResult)
      case _ =>
        // otherwise, build the lens as a new SelectFromOutput node
        Expr.SelectFromOutput(returnOutput, lens, captureResult)
    }
  }

  def get[X](
    buildLens: NamedLens.Fn[R, X],
  )(implicit
    captureResult: CaptureResult[X],
  ): ValExprBuilder[V, X, P] =
    new ValExprBuilder(buildGetExpr[Id, X](buildLens))

  // TODO: Rename? Is it always foldable?
  def getFoldable[N[_], X](
    buildLens: NamedLens.Fn[R, N[X]],
  )(implicit
    captureResult: CaptureResult[N[X]],
  ): FoldableExprBuilder[V, N, X, P] =
    new FoldableExprBuilder(buildGetExpr(buildLens))

  def value[X](
    implicit
    ev: R <:< TypedFact[X],
    captureResult: CaptureResult[X],
  ): ValExprBuilder[V, X, P] =
    new ValExprBuilder(buildGetExpr[Id, X](_.field("value", _.value)))

  def in(accepted: Set[R])(implicit captureCond: CaptureCondResult): ValExprBuilder[V, Boolean, P] =
    new ValExprBuilder(Expr.OutputWithinSet(returnOutput, accepted, captureCond))

  def +(
    rhs: Expr[V, R, P],
  )(implicit
    R: Addition[R],
    captureResult: CaptureResult[R],
  ): ValExprBuilder[V, R, P] =
    new ValExprBuilder(ExprDsl.add(returnOutput, rhs))

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

  def *(
    rhs: Expr[V, R, P],
  )(implicit
    R: Multiplication[R],
    captureResult: CaptureResult[R],
  ): ValExprBuilder[V, R, P] =
    new ValExprBuilder(ExprDsl.multiply(returnOutput, rhs))

  def *(
    rhs: R,
  )(implicit
    R: Multiplication[R],
    captureResult: CaptureResult[R],
  ): ValExprBuilder[V, R, P] =
    new ValExprBuilder(ExprDsl.multiply(returnOutput, Expr.ConstOutput(rhs, Evidence.none, captureResult)))

  def multiplyTo(
    lhs: R,
  )(implicit
    R: Multiplication[R],
    captureResult: CaptureResult[R],
  ): ValExprBuilder[V, R, P] =
    new ValExprBuilder(ExprDsl.multiply(Expr.ConstOutput(lhs, Evidence.none, captureResult), returnOutput))

  def -(
    rhs: Expr[V, R, P],
  )(implicit
    R: Subtraction[R],
    captureResult: CaptureResult[R],
  ): ValExprBuilder[V, R, P] =
    new ValExprBuilder(ExprDsl.subtract(returnOutput, rhs))

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

  def /(
    rhs: Expr[V, R, P],
  )(implicit
    R: Division[R],
    captureResult: CaptureResult[R],
  ): ValExprBuilder[V, R, P] =
    new ValExprBuilder(ExprDsl.divide(returnOutput, rhs))

  def /(
    rhs: R,
  )(implicit
    R: Division[R],
    captureResult: CaptureResult[R],
  ): ValExprBuilder[V, R, P] =
    new ValExprBuilder(ExprDsl.divide(returnOutput, Expr.ConstOutput(rhs, Evidence.none, captureResult)))

  def divideFrom(
    lhs: R,
  )(implicit
    R: Division[R],
    captureResult: CaptureResult[R],
  ): ValExprBuilder[V, R, P] =
    new ValExprBuilder(ExprDsl.divide(Expr.ConstOutput(lhs, Evidence.none, captureResult), returnOutput))

  def unary_-(
    implicit
    R: Negative[R],
    captureResult: CaptureResult[R],
  ): ValExprBuilder[V, R, P] =
    new ValExprBuilder(ExprDsl.negative(returnOutput))

  def within(
    window: Window[R],
  )(implicit
    captureResult: CaptureCondResult,
  ): ValExprBuilder[V, Boolean, P] =
    new ValExprBuilder(ExprDsl.within(returnOutput, window))

  def isEqualTo(
    value: R,
  )(implicit
    orderR: Order[R],
    captureResult: CaptureCondResult,
  ): ValExprBuilder[V, Boolean, P] =
    within(Window.equalTo(value))

  def ===(
    value: R,
  )(implicit
    orderR: Order[R],
    captureResult: CaptureCondResult,
  ): ValExprBuilder[V, Boolean, P] =
    within(Window.equalTo(value))

  def !==(
    value: R,
  )(implicit
    orderR: Order[R],
    captureResult: CaptureCondResult,
  ): ValExprBuilder[V, Boolean, P] =
    Expr.Not(within(Window.equalTo(value)), captureResult)

  def <(
    value: R,
  )(implicit
    orderR: Order[R],
    captureResult: CaptureCondResult,
  ): ValExprBuilder[V, Boolean, P] =
    within(Window.lessThan(value))

  def <=(
    value: R,
  )(implicit
    orderR: Order[R],
    captureResult: CaptureCondResult,
  ): ValExprBuilder[V, Boolean, P] =
    within(Window.lessThanOrEqual(value))

  def >(
    value: R,
  )(implicit
    orderR: Order[R],
    captureResult: CaptureCondResult,
  ): ValExprBuilder[V, Boolean, P] =
    within(Window.greaterThan(value))

  def >=(
    value: R,
  )(implicit
    orderR: Order[R],
    captureResult: CaptureCondResult,
  ): ValExprBuilder[V, Boolean, P] =
    within(Window.greaterThanOrEqual(value))
}
