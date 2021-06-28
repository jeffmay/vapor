package com.rallyhealth

package vapors.dsl

import vapors.algebra.{CaptureP, Expr, ExprConverter, ExprSorter}
import vapors.data.{Evidence, FactTable, TypedFact, Window}
import vapors.lens.NamedLens
import vapors.math._

import cats._

import scala.collection.{Factory, MapView, View}
import scala.reflect.runtime.universe.TypeTag

/**
  * Chains algebraic operations into a single [[Expr]] node.
  *
  * @param returnOutput the head of the tree of expressions as built so far
  *
  * TODO: Make this more reusable for [[MapViewExprBuilder]]
  */
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

/**
  * Same as [[ExprBuilder]], but for arity-1 higher-kinded types
  *
  * @see [[ExprBuilder]]
  *
  * TODO: Rename? Is this always foldable?
  */
final class FoldableExprBuilder[V, M[_], U, P](returnOutput: Expr[V, M[U], P])
  extends ExprBuilder[V, M, U, P](returnOutput) {

  /**
    * Same as [[to]](List).
    */
  def toList(
    implicit
    ev: M[U] <:< IterableOnce[U],
    captureResult: CaptureResult[List[U]],
  ): FoldableExprBuilder[V, List, U, P] =
    to(List)

  /**
    * Same as [[to]](Set).
    */
  def toSet(
    implicit
    ev: M[U] <:< IterableOnce[U],
    captureResult: CaptureResult[Set[U]],
  ): FoldableExprBuilder[V, Set, U, P] =
    to(Set)

  /**
    * Converts the iterable of 2-tuples return value into a Map.
    *
    * @example
    * {{{
    *   assert(eval(const(Set("A" -> 1, "B" -> 2)).withOutputFoldable.toMap).output.value == Map("A" -> 1, "B" -> 2))
    * }}}
    */
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

  /**
    * Converts the iterable return value into the specific Scala collection companion object.
    *
    * @example
    * {{{
    *   assert(eval(const(List(1, 2, 2, 3)).withOutputFoldable.to(Set)).output.value == Set(1, 2, 3))
    * }}}
    */
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

  /**
    * Sorts the sequence of values returned based on the given total ordering for the value type.
    */
  def sorted(
    implicit
    orderU: Order[U],
    ev: M[U] <:< Seq[U],
    tt: TypeTag[U],
    factory: Factory[U, M[U]],
    captureResult: CaptureResult[M[U]],
  ): FoldableExprBuilder[V, M, U, P] =
    new FoldableExprBuilder(Expr.SortOutput(returnOutput, ExprSorter.byNaturalOrder[M, U], captureResult))

  /**
    * Sorts the sequence of values returned based on the given total ordering for the type of the
    * selected field of the values.
    */
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

  /**
    * Groups the [[Foldable]] return value by the selected field.
    *
    * The values of the map produced must have a total ordering so that they are retrieved from the map
    * in a consistent and predictable way.
    */
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

  /**
    * Takes a given number of elements from the start of the traversable return value.
    */
  def take(
    n: Int,
  )(implicit
    traverseM: Traverse[M],
    traverseFilterM: TraverseFilter[M],
    captureResult: CaptureResult[M[U]],
  ): FoldableExprBuilder[V, M, U, P] =
    new FoldableExprBuilder(Expr.TakeFromOutput(returnOutput, n, captureResult))

  /**
    * Takes the first element from the start of the traversable return value or None, if the return value is empty.
    */
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

  /**
    * Maps an expression over every element of the returned [[Functor]] and folds the evidence together.
    */
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

  /**
    * Maps an expression that produces the same return type constructor and flattens all the returned
    * elements together based on the definition of [[FlatMap]].
    *
    * This will also fold all the evidence together.
    *
    * @note there is no good definition of `flatMap` inside of an applicative data structure. The input
    *       to any outer expression builder almost always not be the same type as the type of input to
    *       the inner expression to be flattened. This means that you can't reference any expression outside
    *       of the scope of the expression builder function given to `.flatMap`.
    *
    *       The only way to fix this would be to provide some way to carry the input from previous expressions
    *       around in some kind of stack or grab bag, from which you could grab values for another expression.
    *       Until then, this method is kind of useless.
    */
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

  /**
    * Fold all the result values into a single result based on the definition of [[Monoid]].
    *
    * @example
    * {{{
    *   // for better or for worse, the default definition of Monoid[Int] is summation
    *   assert(eval(const(List(1, 2, 3)).withOutputFoldable.fold).output.value == 6)
    * }}}
    */
  def fold(
    implicit
    foldableM: Foldable[M],
    monoidU: Monoid[U],
    captureResult: CaptureResult[U],
  ): ValExprBuilder[V, U, P] = {
    new ValExprBuilder(Expr.FoldOutput(returnOutput, captureResult))
  }

  /**
    * Returns true if the result value is an empty [[Foldable]].
    */
  def isEmpty(
    implicit
    foldableM: Foldable[M],
    captureResult: CaptureCondResult,
  ): ValExprBuilder[V, Boolean, P] =
    new ValExprBuilder(Expr.OutputIsEmpty(returnOutput, captureResult))

  /**
    * Computes the given condition expression for every element of the returned value until one returns true
    * OR the returned value is empty. Otherwise -- if all elements of the non-empty returned value return false
    * -- this expression will return false.
    */
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

  /**
    * Filters out all elements of the returned [[Foldable]] for which the given conditional expression returns false.
    *
    * What's left is all the elements of this expression's returned [[Foldable]] for which the given condition
    * returns true.
    */
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

  /**
    * Returns true if the returned [[Foldable]] shares any elements in common with the given set of valid values.
    */
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

/**
  * Same as [[ExprBuilder]], but for concrete types.
  *
  * @see [[ExprBuilder]]
  *
  * TODO: This should probably share some methods with [[FoldableExprBuilder]]... or at least
  *       switching between builder types should be a lot easier.
  */
final class ValExprBuilder[V, R, P](override val returnOutput: Expr[V, R, P])
  extends ExprBuilder[V, Id, R, P](returnOutput) {

  // chains expression builder .get calls into a single select operation
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

  /**
    * Selects a field from the returned value using the given lens.
    *
    * @example
    * {{{
    *   // assuming you run this example in 2021...
    *   assert(const(LocalDate.now()).withOutputValue.get(_.select(_.getYear)).output.value == 2021)
    * }}}
    */
  def get[X](
    buildLens: NamedLens.Fn[R, X],
  )(implicit
    captureResult: CaptureResult[X],
  ): ValExprBuilder[V, X, P] =
    new ValExprBuilder(buildGetExpr[Id, X](buildLens))

  /**
    * Same as [[get]], except it expects the selected value to have a higher-kinded type and returns
    * a [[FoldableExprBuilder]].
    *
    * @example
    * {{{
    *   // selecting a value from a Map returns an Option, which is foldable
    *   assert(const(Map("A" -> 1)).withOutputValue.getFoldable(_.at("A")).isEmpty).output.value == false)
    * }}}
    *
    * TODO: Rename? It only needs to be a higher-kinded type, but [[FoldableExprBuilder]] almost always
    *       requires a [[Foldable]] definition for any chained operations. Ideally, we would be able
    *       to avoid needing to distinguish the type of builder to use until after we have selected the field.
    */
  def getFoldable[N[_], X](
    buildLens: NamedLens.Fn[R, N[X]],
  )(implicit
    captureResult: CaptureResult[N[X]],
  ): FoldableExprBuilder[V, N, X, P] =
    new FoldableExprBuilder(buildGetExpr(buildLens))

  @deprecated(
    "Use valuesOfType(...) to avoid needing this .value method or use .get(_.select(_.value)) instead.",
    "0.17.0",
  )
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

  @deprecated(
    "Use + const(...) instead. This overloaded convenience method will be removed to support better type inference",
    "0.17.0",
  )
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

  @deprecated(
    "Use * const(...) instead. This overloaded convenience method will be removed to support better type inference",
    "0.17.0",
  )
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

  @deprecated(
    "Use - const(...) instead. This overloaded convenience method will be removed to support better type inference",
    "0.17.0",
  )
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

  @deprecated(
    "Use / const(...) instead. This overloaded convenience method will be removed to support better type inference",
    "0.17.0",
  )
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

  def withinWindow(
    windowExpr: Expr[V, Window[R], P],
  )(implicit
    captureResult: CaptureCondResult,
  ): ValExprBuilder[V, Boolean, P] =
    new ValExprBuilder(ExprDsl.within(returnOutput, windowExpr))

  def within(
    window: Window[R],
  )(implicit
    captureWindow: CaptureP[V, Window[R], P],
    captureConst: CaptureP[FactTable, Window[R], P],
    captureResult: CaptureCondResult,
  ): ValExprBuilder[V, Boolean, P] =
    new ValExprBuilder(
      Expr.OutputWithinWindow(returnOutput, embed(const(window)(captureConst))(captureWindow), captureResult),
    )

  @deprecated("Use === const(...) instead. This method doesn't fit with the overall DSL.", "0.17.0")
  def isEqualTo(
    value: R,
  )(implicit
    orderR: Order[R],
    captureWindow: CaptureP[V, Window[R], P],
    captureConst: CaptureP[FactTable, Window[R], P],
    captureResult: CaptureCondResult,
  ): ValExprBuilder[V, Boolean, P] =
    within(Window.equalTo(value))

  @deprecated(
    "Use === const(...) instead. This overloaded convenience method will be removed to support better type inference",
    "0.17.0",
  )
  def ===(
    value: R,
  )(implicit
    orderR: Order[R],
    captureWindow: CaptureP[V, Window[R], P],
    captureConst: CaptureP[FactTable, Window[R], P],
    captureResult: CaptureCondResult,
  ): ValExprBuilder[V, Boolean, P] =
    within(Window.equalTo(value))

  def ===(
    valueExpr: Expr[V, R, P],
  )(implicit
    orderR: Order[R],
    captureWindow: CaptureP[V, Window[R], P],
    captureResult: CaptureCondResult,
  ): ValExprBuilder[V, Boolean, P] =
    withinWindow(Expr.WrapOutput(valueExpr, ExprConverter.asWindow[R](Window.equalTo(_)), captureWindow))

  @deprecated(
    "Use !== const(...) instead. This overloaded convenience method will be removed to support better type inference",
    "0.17.0",
  )
  def !==(
    value: R,
  )(implicit
    orderR: Order[R],
    captureWindow: CaptureP[V, Window[R], P],
    captureConst: CaptureP[FactTable, Window[R], P],
    captureResult: CaptureCondResult,
  ): ValExprBuilder[V, Boolean, P] =
    Expr.Not(within(Window.equalTo(value)), captureResult)

  def !==(
    valueExpr: Expr[V, R, P],
  )(implicit
    orderR: Order[R],
    captureWindow: CaptureP[V, Window[R], P],
    captureResult: CaptureCondResult,
  ): ValExprBuilder[V, Boolean, P] =
    Expr.Not(this === valueExpr, captureResult)

  @deprecated(
    "Use < const(...) instead. This overloaded convenience method will be removed to support better type inference",
    "0.17.0",
  )
  def <(
    value: R,
  )(implicit
    orderR: Order[R],
    captureWindow: CaptureP[V, Window[R], P],
    captureConst: CaptureP[FactTable, Window[R], P],
    captureResult: CaptureCondResult,
  ): ValExprBuilder[V, Boolean, P] =
    within(Window.lessThan(value))

  def <(
    valueExpr: Expr[V, R, P],
  )(implicit
    orderR: Order[R],
    captureWindow: CaptureP[V, Window[R], P],
    captureResult: CaptureCondResult,
  ): ValExprBuilder[V, Boolean, P] =
    withinWindow(Expr.WrapOutput(valueExpr, ExprConverter.asWindow[R](Window.lessThan(_)), captureWindow))

  @deprecated(
    "Use <= const(...) instead. This overloaded convenience method will be removed to support better type inference",
    "0.17.0",
  )
  def <=(
    value: R,
  )(implicit
    orderR: Order[R],
    captureWindow: CaptureP[V, Window[R], P],
    captureConst: CaptureP[FactTable, Window[R], P],
    captureResult: CaptureCondResult,
  ): ValExprBuilder[V, Boolean, P] =
    within(Window.lessThanOrEqual(value))

  def <=(
    valueExpr: Expr[V, R, P],
  )(implicit
    orderR: Order[R],
    captureWindow: CaptureP[V, Window[R], P],
    captureResult: CaptureCondResult,
  ): ValExprBuilder[V, Boolean, P] =
    withinWindow(Expr.WrapOutput(valueExpr, ExprConverter.asWindow[R](Window.lessThanOrEqual(_)), captureWindow))

  @deprecated(
    "Use > const(...) instead. This overloaded convenience method will be removed to support better type inference",
    "0.17.0",
  )
  def >(
    value: R,
  )(implicit
    orderR: Order[R],
    captureWindow: CaptureP[V, Window[R], P],
    captureConst: CaptureP[FactTable, Window[R], P],
    captureResult: CaptureCondResult,
  ): ValExprBuilder[V, Boolean, P] =
    within(Window.greaterThan(value))

  def >(
    valueExpr: Expr[V, R, P],
  )(implicit
    orderR: Order[R],
    captureWindow: CaptureP[V, Window[R], P],
    captureResult: CaptureCondResult,
  ): ValExprBuilder[V, Boolean, P] =
    withinWindow(Expr.WrapOutput(valueExpr, ExprConverter.asWindow[R](Window.greaterThan(_)), captureWindow))

  @deprecated(
    "Use >= const(...) instead. This overloaded convenience method will be removed to support better type inference",
    "0.17.0",
  )
  def >=(
    value: R,
  )(implicit
    orderR: Order[R],
    captureWindow: CaptureP[V, Window[R], P],
    captureConst: CaptureP[FactTable, Window[R], P],
    captureResult: CaptureCondResult,
  ): ValExprBuilder[V, Boolean, P] =
    within(Window.greaterThanOrEqual(value))

  def >=(
    valueExpr: Expr[V, R, P],
  )(implicit
    orderR: Order[R],
    captureWindow: CaptureP[V, Window[R], P],
    captureResult: CaptureCondResult,
  ): ValExprBuilder[V, Boolean, P] =
    withinWindow(Expr.WrapOutput(valueExpr, ExprConverter.asWindow[R](Window.greaterThanOrEqual(_)), captureWindow))
}
