package com.rallyhealth

package vapors.dsl

import vapors.algebra.{CaptureP, Expr, ExprResult}
import vapors.data._
import vapors.interpreter.{ExprInput, InterpretExprAsResultFn}
import vapors.lens.NamedLens
import vapors.logic.{Conjunction, Disjunction, Negation}
import vapors.math._

import cats.data.NonEmptyList
import cats.{Foldable, Monoid}

object ExprDsl extends ExprDsl

// TODO: Remove methods that are not useful anymore and move less-useful methods to a separate object
trait ExprDsl extends TimeFunctions with WrapExprSyntax with WrapEachExprSyntax {

  /**
    * Evaluates the given [[RootExpr]] with the given [[FactTable]] to produce the [[ExprResult]].
    *
    * To access the value of the result, you can get the `result.output.value`. [[Evidence]] for the
    * value can be accessed from `result.output.evidence`. Any parameter captured in [[CaptureP]] is
    * accessible from `result.param`.
    *
    * You can also apply a post-processing [[ExprResult.Visitor]] to analyze the results in more depth
    * in a recursive and type-safe way.
    */
  def eval[R, P](facts: FactTable)(query: RootExpr[R, P]): ExprResult[FactTable, R, P] = {
    InterpretExprAsResultFn(query)(ExprInput.fromFactTable(facts))
  }

  /**
    * Creates an embeddable [[RootExpr]] that always returns a given constant value.
    *
    * @see [[Expr.ConstOutput]]
    */
  def const[R, P](
    value: R,
    evidence: Evidence = Evidence.none,
  )(implicit
    capture: CaptureRootExpr[R, P],
  ): Expr.ConstOutput[FactTable, R, P] =
    Expr.ConstOutput(value, evidence, capture)

  /**
    * Uses the value of a given fact and also considers the fact as evidence of its own value.
    */
  def factValue[R, P](
    typedFact: TypedFact[R],
  )(implicit
    capture: CaptureRootExpr[R, P],
  ): Expr.ConstOutput[FactTable, R, P] =
    const(typedFact.value, Evidence(typedFact))

  /**
    * @note For better type inference, you should consider using [[ExprBuilder.returnInput]]
    *
    * This method may be deprecated in the future.
    *
    * @see [[Expr.ReturnInput]]
    */
  def input[V, P](
    implicit
    capture: CaptureP[V, V, P],
  ): Expr.ReturnInput[V, P] =
    Expr.ReturnInput(capture)

  /**
    * Start the creation of an [[Expr.Define]] for a [[FactType]].
    */
  def define[T](factType: FactType[T]): DefinitionExprBuilder[T] = new DefinitionExprBuilder(factType)

  /**
    * Apply the given definitions to add the produced [[Fact]]s to the [[FactTable]] for the life of the
    * given `subExpr`.
    *
    * @param definitions a set of [[Expr.Definition]]s that will all be concatenated to the [[FactTable]]
    * @param subExpr an expression that will see the defined [[Fact]]s in its [[FactTable]]
    */
  def usingDefinitions[V, R, P](
    definitions: Expr.Definition[P]*,
  )(
    subExpr: Expr[V, R, P],
  )(implicit
    captureResult: CaptureP[V, R, P],
  ): Expr.UsingDefinitions[V, R, P] =
    Expr.UsingDefinitions(definitions.toVector, subExpr, captureResult)

  /**
    * Typically, this function will be applied implicitly by the DSL. If it doesn't you may have to explicitly
    * choose the type to embed. For better type inference, you should use [[ExprBuilder.embedConst]].
    *
    * This method may be deprecated in the future.
    *
    * @see [[Expr.ConstOutput]]
    */
  def embed[V, R, P](
    expr: RootExpr[R, P],
  )(implicit
    captureResult: CaptureP[V, R, P],
  ): Expr.Embed[V, R, P] =
    Expr.Embed(expr, captureResult)

  /**
    * Concatenates the outputs of the expressions given.
    */
  def concat[V, M[_], R, P](expressions: Expr[V, M[R], P]*): ConcatOutputExprBuilder[V, M, R, P] =
    new ConcatOutputExprBuilder(expressions.to(LazyList))

  /**
    * @see [[Expr.And]] for more details of how [[Conjunction]] works during evaluation.
    */
  def and[V, R : Conjunction : ExtractBoolean, P](
    first: Expr[V, R, P],
    second: Expr[V, R, P],
    remaining: Expr[V, R, P]*,
  )(implicit
    captureResult: CaptureP[V, R, P],
  ): Expr.And[V, R, P] = {
    val subExpressions = first :: NonEmptyList.of(second, remaining: _*)
    Expr.And(subExpressions, captureResult)
  }

  /**
    * @see [[Expr.Or]] for more details of how [[Disjunction]] works during evaluation.
    */
  def or[V, R : Disjunction : ExtractBoolean, P](
    first: Expr[V, R, P],
    second: Expr[V, R, P],
    remaining: Expr[V, R, P]*,
  )(implicit
    captureResult: CaptureP[V, R, P],
  ): Expr.Or[V, R, P] = {
    val subExpressions = first :: NonEmptyList.of(second, remaining: _*)
    Expr.Or(subExpressions, captureResult)
  }

  /**
    * @see [[Expr.Not]] for more details of how [[Negation]] works during evaluation.
    */
  def not[V, R : Negation, P](
    sub: Expr[V, R, P],
  )(implicit
    captureResult: CaptureP[V, R, P],
  ): Expr.Not[V, R, P] =
    Expr.Not(sub, captureResult)

  /**
    * Builds an if / then / elif / else style conditional branching expression.
    */
  def when[V, R, P](condExpr: CondExpr[V, P]): WhenExprBuilder[V, P] = new WhenExprBuilder(condExpr)

  /**
    * Grabs all facts of a given set of types and returns them in order of fact type alphabetically,
    * then by their natural ordering (as defined on the [[FactType]]).
    */
  def factsOfType[T, P](
    factTypeSet: FactTypeSet[T],
  )(implicit
    captureInput: CaptureFromFacts[T, P],
    captureAllResults: CaptureRootExpr[Seq[TypedFact[T]], P],
  ): FoldableExprBuilder[FactTable, Seq, TypedFact[T], P] =
    new FoldableExprBuilder(
      Expr.WithFactsOfType[T, Seq[TypedFact[T]], P](
        factTypeSet,
        input(captureInput),
        captureAllResults,
      ),
    )

  /**
    * Same as [[factsOfType]], but maps the facts into their values.
    */
  def valuesOfType[T, P](
    factTypeSet: FactTypeSet[T],
  )(implicit
    captureInput: CaptureFromFacts[T, P],
    captureAllFacts: CaptureRootExpr[Seq[TypedFact[T]], P],
    captureEachResult: CaptureP[TypedFact[T], T, P],
    captureEachInput: CaptureP[TypedFact[T], TypedFact[T], P],
    captureAllResults: CaptureP[FactTable, Seq[T], P],
  ): FoldableExprBuilder[FactTable, Seq, T, P] =
    factsOfType(factTypeSet).map(_.value)

  /**
    * Takes a sequence of expressions and produces an expression of sequence of all the items.
    *
    * @see [[wrapSeq]] for varargs syntactic sugar.
    *
    *      If you would like to put the results into a heterogeneous sequence, then you should use the [[wrap]] method.
    */
  def sequence[V, R, P](
    expressions: Seq[Expr[V, R, P]],
  )(implicit
    captureResult: CaptureP[V, Seq[R], P],
  ): Expr.WrapOutputSeq[V, R, P] = Expr.WrapOutputSeq(expressions, captureResult)

  /**
    * Syntactic sugar for [[sequence]].
    *
    * You should use this method over [[sequence]] when you are writing your expression in the embedded DSL.
    * If you are abstracting over a sequence of expressions, then you should use [[sequence]].
    */
  @inline final def wrapSeq[V, R, P](
    expressions: Expr[V, R, P]*,
  )(implicit
    captureResult: CaptureP[V, Seq[R], P],
  ): Expr.WrapOutputSeq[V, R, P] = sequence(expressions)

  /*
   * The following DSL functions will probably be deprecated in favor of their ExprBuilder equivalents
   */

  def collectSome[V, M[_] : Foldable, U, R : Monoid, P](
    inputExpr: Expr[V, M[U], P],
    collectExpr: ValExpr[U, Option[R], P],
  )(implicit
    capture: CaptureP[V, R, P],
  ): Expr.CollectFromOutput[V, M, U, R, P] =
    Expr.CollectFromOutput(inputExpr, collectExpr, capture)

  def selectFrom[V, S, R, P](
    inputExpr: Expr[V, S, P],
    lens: NamedLens[S, R],
  )(implicit
    captureInput: CaptureP[V, R, P],
  ): Expr.SelectFromOutput[V, S, R, P] =
    Expr.SelectFromOutput(inputExpr, lens, captureInput)

  def add[V, R : Addition, P](
    lhs: Expr[V, R, P],
    rhs: Expr[V, R, P],
  )(implicit
    capture: CaptureP[V, R, P],
  ): Expr.AddOutputs[V, R, P] =
    Expr.AddOutputs(NonEmptyList.of(lhs, rhs), capture)

  def multiply[V, R : Multiplication, P](
    lhs: Expr[V, R, P],
    rhs: Expr[V, R, P],
  )(implicit
    capture: CaptureP[V, R, P],
  ): Expr.MultiplyOutputs[V, R, P] =
    Expr.MultiplyOutputs(NonEmptyList.of(lhs, rhs), capture)

  def subtract[V, R : Subtraction, P](
    lhs: Expr[V, R, P],
    rhs: Expr[V, R, P],
  )(implicit
    capture: CaptureP[V, R, P],
  ): Expr.SubtractOutputs[V, R, P] =
    Expr.SubtractOutputs(NonEmptyList.of(lhs, rhs), capture)

  def divide[V, R : Division, P](
    lhs: Expr[V, R, P],
    rhs: Expr[V, R, P],
  )(implicit
    capture: CaptureP[V, R, P],
  ): Expr.DivideOutputs[V, R, P] =
    Expr.DivideOutputs(NonEmptyList.of(lhs, rhs), capture)

  def negative[V, R : Negative, P](
    inputExpr: Expr[V, R, P],
  )(implicit
    capture: CaptureP[V, R, P],
  ): Expr.NegativeOutput[V, R, P] =
    Expr.NegativeOutput(inputExpr, capture)

  def exists[V, M[_] : Foldable, U, P](
    inputExpr: Expr[V, M[U], P],
    condExpr: ValCondExpr[U, P],
  )(implicit
    capture: CaptureP[V, Boolean, P],
  ): Expr.ExistsInOutput[V, M, U, P] =
    Expr.ExistsInOutput(inputExpr, condExpr, capture)

  def returnInput[V, P](
    implicit
    capture: CaptureP[V, V, P],
  ): Expr.ReturnInput[V, P] =
    Expr.ReturnInput(capture)

  def within[V, R, P](
    inputExpr: Expr[V, R, P],
    windowExpr: Expr[V, Window[R], P],
  )(implicit
    capture: CaptureP[V, Boolean, P],
  ): Expr.OutputWithinWindow[V, R, P] =
    Expr.OutputWithinWindow(inputExpr, windowExpr, capture)
}
