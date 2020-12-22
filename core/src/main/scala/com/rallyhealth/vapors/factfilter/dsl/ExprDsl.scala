package com.rallyhealth.vapors.factfilter.dsl

import cats.data.NonEmptyList
import cats.{Foldable, Id, Monoid}
import com.rallyhealth.vapors.core.algebra.Expr.Definition
import com.rallyhealth.vapors.core.algebra.{Expr, ExprResult}
import com.rallyhealth.vapors.core.data.{NamedLens, Window}
import com.rallyhealth.vapors.core.logic.{Conjunction, Disjunction, Negation}
import com.rallyhealth.vapors.core.math.{Addition, Division, Multiplication, Negative, Subtraction}
import com.rallyhealth.vapors.factfilter.data._
import com.rallyhealth.vapors.factfilter.evaluator.InterpretExprAsFunction

import scala.collection.immutable.SortedSet

object ExprDsl extends ExprBuilderSyntax {

  final type CondExpr[F[_], V, P] = Expr[F, V, Boolean, P]

  final type ValExpr[V, R, P] = Expr[Id, V, R, P]
  final type ValCondExpr[V, P] = ValExpr[V, Boolean, P]

  type RootExpr[R, P] = Expr[Id, FactTable, R, P]

  type CaptureRootExpr[R, P] = CaptureP[Id, FactTable, R, P]
  type CaptureFromFacts[T, P] = CaptureP[SortedSet, TypedFact[T], SortedSet[TypedFact[T]], P]

  import InterpretExprAsFunction._

  def eval[R, P](facts: FactTable)(query: RootExpr[R, P]): ExprResult[Id, FactTable, R, P] = {
    InterpretExprAsFunction(query)(Input.fromFactTable(facts))
  }

  /**
    * Lifts the given value into the output of an expression with no evidence.
    */
  def const[F[_], V, R, P](
    value: R,
    evidence: Evidence = Evidence.none,
  )(implicit
    post: CaptureP[F, V, R, P],
  ): Expr[F, V, R, P] =
    Expr.ConstOutput(value, evidence, post)

  /**
    * Uses the value of a given fact and also considers the fact as evidence of its own value.
    */
  def factValue[F[_], V, R, P](
    typedFact: TypedFact[R],
  )(implicit
    post: CaptureP[F, V, R, P],
  ): Expr[F, V, R, P] =
    const(typedFact.value, Evidence(typedFact))

  def input[F[_], V, P](
    implicit
    post: CaptureP[F, V, F[V], P],
  ): Expr[F, V, F[V], P] =
    Expr.ReturnInput(post)

  def define[M[_] : Foldable, T, P](
    factType: FactType[T],
  )(
    defExpr: RootExpr[M[T], P],
  )(implicit
    postResult: CaptureRootExpr[FactSet, P],
  ): Definition[P] =
    Expr.Define(factType, defExpr, postResult)

  def usingDefinitions[F[_], V, R, P](
    definitions: Expr.Definition[P]*,
  )(
    subExpr: Expr[F, V, R, P],
  )(implicit
    postResult: CaptureP[F, V, R, P],
  ): Expr[F, V, R, P] = {
    Expr.UsingDefinitions(definitions.toVector, subExpr, postResult)
  }

  /**
    * Implicitly allows embedding any expression that only requires the FactTable into expression.
    *
    * @see [[Expr.Embed]]
    */
  implicit def embed[F[_], V, R, P](
    expr: RootExpr[R, P],
  )(implicit
    postResult: CaptureP[F, V, R, P],
  ): Expr[F, V, R, P] = Expr.Embed(expr, postResult)

  def and[F[_], V, R : Conjunction : ExtractBoolean, P](
    first: Expr[F, V, R, P],
    second: Expr[F, V, R, P],
    remaining: Expr[F, V, R, P]*,
  )(implicit
    postResult: CaptureP[F, V, R, P],
  ): Expr[F, V, R, P] = {
    val subExpressions = first :: NonEmptyList.of(second, remaining: _*)
    Expr.And(subExpressions, postResult)
  }

  def or[F[_], V, R : Disjunction : ExtractBoolean, P](
    first: Expr[F, V, R, P],
    second: Expr[F, V, R, P],
    remaining: Expr[F, V, R, P]*,
  )(implicit
    postResult: CaptureP[F, V, R, P],
  ): Expr[F, V, R, P] = {
    val subExpressions = first :: NonEmptyList.of(second, remaining: _*)
    Expr.Or(subExpressions, postResult)
  }

  def not[F[_], V, R : Negation, P](
    sub: Expr[F, V, R, P],
  )(implicit
    postResult: CaptureP[F, V, R, P],
  ): Expr.Not[F, V, R, P] =
    Expr.Not(sub, postResult)

  def when[F[_], V, R, P](condExpr: CondExpr[F, V, P]): WhenBuilder[F, V, P] = new WhenBuilder(condExpr)

  final class WhenBuilder[F[_], V, P](private val condExpr: CondExpr[F, V, P]) extends AnyVal {

    def thenReturn[R](thenExpr: Expr[F, V, R, P]): WhenThenBuilder[F, V, R, P] = new WhenThenBuilder(condExpr, thenExpr)
  }

  final class WhenThenBuilder[F[_], V, R, P](
    condExpr: CondExpr[F, V, P],
    thenExpr: Expr[F, V, R, P],
  ) {

    def elseReturn(
      elseExpr: Expr[F, V, R, P],
    )(implicit
      postResult: CaptureP[F, V, R, P],
    ): Expr[F, V, R, P] =
      Expr.When(condExpr, thenExpr, elseExpr, postResult)
  }

  def withFactsOfType[T, P](
    factTypeSet: FactTypeSet[T],
  )(implicit
    postInput: CaptureFromFacts[T, P],
  ): FactsOfTypeBuilder[T, P] =
    new FactsOfTypeBuilder(factTypeSet)

  final class FactsOfTypeBuilder[T, P](
    factTypeSet: FactTypeSet[T],
  )(implicit
    postInput: CaptureFromFacts[T, P],
  ) {

    def where[M[_], U](
      buildSubExpr: ExprBuilder.FoldableFn[SortedSet, TypedFact[T], M, U, P],
    )(implicit
      postResult: CaptureRootExpr[M[U], P],
    ): RootExpr[M[U], P] = {
      Expr.WithFactsOfType(
        factTypeSet,
        buildSubExpr(new FoldableExprBuilder(ExprDsl.input)).returnOutput,
        postResult,
      )
    }

    def returnInput(
      implicit
      postInput: CaptureFromFacts[T, P],
      postResult: CaptureRootExpr[SortedSet[TypedFact[T]], P],
    ): RootExpr[SortedSet[TypedFact[T]], P] =
      Expr.WithFactsOfType(factTypeSet, input(postInput), postResult)
  }

  def collectSome[F[_], V, M[_] : Foldable, U, R : Monoid, P](
    inputExpr: Expr[F, V, M[U], P],
    collectExpr: ValExpr[U, Option[R], P],
  )(implicit
    post: CaptureP[F, V, R, P],
  ): Expr[F, V, R, P] =
    Expr.CollectFromOutput(inputExpr, collectExpr, post)

  def selectFrom[F[_], V, S, R, P](
    inputExpr: Expr[F, V, S, P],
    lens: NamedLens[S, R],
  )(implicit
    postInput: CaptureP[F, V, R, P],
  ): Expr[F, V, R, P] =
    Expr.SelectFromOutput(inputExpr, lens, postInput)

  def add[F[_], V, R : Addition, P](
    lhs: Expr[F, V, R, P],
    rhs: Expr[F, V, R, P],
  )(implicit
    post: CaptureP[F, V, R, P],
  ): Expr[F, V, R, P] =
    Expr.AddOutputs(NonEmptyList.of(lhs, rhs), post)

  def subtract[F[_], V, R : Subtraction, P](
    lhs: Expr[F, V, R, P],
    rhs: Expr[F, V, R, P],
  )(implicit
    post: CaptureP[F, V, R, P],
  ): Expr[F, V, R, P] =
    Expr.SubtractOutputs(NonEmptyList.of(lhs, rhs), post)

  def multiply[F[_], V, R : Multiplication, P](
    lhs: Expr[F, V, R, P],
    rhs: Expr[F, V, R, P],
  )(implicit
    post: CaptureP[F, V, R, P],
  ): Expr[F, V, R, P] =
    Expr.MultiplyOutputs(NonEmptyList.of(lhs, rhs), post)

  def divide[F[_], V, R : Division, P](
    lhs: Expr[F, V, R, P],
    rhs: Expr[F, V, R, P],
  )(implicit
    post: CaptureP[F, V, R, P],
  ): Expr[F, V, R, P] =
    Expr.DivideOutputs(NonEmptyList.of(lhs, rhs), post)

  def negative[F[_], V, R : Negative, P](
    inputExpr: Expr[F, V, R, P],
  )(implicit
    post: CaptureP[F, V, R, P],
  ): Expr[F, V, R, P] =
    Expr.NegativeOutput(inputExpr, post)

  def exists[F[_], V, M[_] : Foldable, U, P](
    inputExpr: Expr[F, V, M[U], P],
    condExpr: ValCondExpr[U, P],
  )(implicit
    post: CaptureP[F, V, Boolean, P],
  ): CondExpr[F, V, P] =
    Expr.ExistsInOutput(inputExpr, condExpr, post)

  def returnInputFoldable[F[_], V, P](
    implicit
    post: CaptureP[F, V, F[V], P],
  ): Expr[F, V, F[V], P] =
    Expr.ReturnInput(post)

  def returnInputValue[V, P](
    implicit
    post: CaptureP[Id, V, V, P],
  ): Expr[Id, V, V, P] =
    Expr.ReturnInput[Id, V, P](post)

  def within[F[_], V, R, P](
    inputExpr: Expr[F, V, R, P],
    window: Window[R],
  )(implicit
    post: CaptureP[F, V, Boolean, P],
  ): Expr[F, V, Boolean, P] =
    Expr.OutputWithinWindow(inputExpr, window, post)
}
