package com.rallyhealth.vapors.core.algebra

import cats.data.NonEmptyList
import cats.kernel.Monoid
import cats.{FlatMap, Foldable, Functor}
import com.rallyhealth.vapors.core.data.{NamedLens, Window}
import com.rallyhealth.vapors.core.logic.{Conjunction, Disjunction, Negation}
import com.rallyhealth.vapors.core.math.{Addition, Division, Multiplication, Negative, Subtraction}
import com.rallyhealth.vapors.factfilter.data._
import com.rallyhealth.vapors.factfilter.dsl.CaptureP

import scala.collection.immutable.SortedSet

/**
  * The core expression algebra.
  *
  * In essence, you can view this as a function from F[V] => R with a custom parameter that you can capture at each
  * point in the expression using a [[CaptureP]].
  *
  * This expression is folded over to produce serialized versions of the expression as well as the evaluator function.
  * @see [[com.rallyhealth.vapors.factfilter.evaluator.InterpretExprAsFunction]]
  *
  * F = Foldable type constructor
  * V = Value type
  * F[V] = the input value type
  * R = Return type
  * P = Captured param
  */
sealed abstract class Expr[F[_], V, R, P] {

  def visit[G[_]](v: Expr.Visitor[F, V, P, G]): G[R]

  def capture: CaptureP[F, V, R, P]
}

object Expr {

  import cats.{~>, Id}

  trait Visitor[F[_], V, P, G[_]] extends (Expr[F, V, *, P] ~> G) {
    // Please keep the following methods in alphabetical order
    override final def apply[R](fa: Expr[F, V, R, P]): G[R] = fa.visit(this)
    def visitAddOutputs[R : Addition](expr: AddOutputs[F, V, R, P]): G[R]
    def visitAnd[R : Conjunction : ExtractBoolean](expr: And[F, V, R, P]): G[R]
    def visitCollectSomeOutput[M[_] : Foldable, U, R : Monoid](expr: CollectFromOutput[F, V, M, U, R, P]): G[R]
    def visitConstOutput[R](expr: ConstOutput[R, P]): G[R]
    def visitDefine[M[_] : Foldable, T](expr: Define[M, T, P]): G[FactSet]
    def visitDivideOutputs[R : Division](expr: DivideOutputs[F, V, R, P]): G[R]
    def visitEmbed[U : ExtractFromAnyInput, R](expr: Embed[F, V, U, R, P]): G[R]
    def visitExistsInOutput[M[_] : Foldable, U](expr: ExistsInOutput[F, V, M, U, P]): G[Boolean]
    def visitFlatMapOutput[M[_] : Foldable : FlatMap, U, X](expr: FlatMapOutput[F, V, M, U, X, P]): G[M[X]]
    def visitMapOutput[M[_] : Foldable : Functor, U, R](expr: MapOutput[F, V, M, U, R, P]): G[M[R]]
    def visitMultiplyOutputs[R : Multiplication](expr: MultiplyOutputs[F, V, R, P]): G[R]
    def visitNegativeOutput[R : Negative](expr: NegativeOutput[F, V, R, P]): G[R]
    def visitNot[R : Negation](expr: Not[F, V, R, P]): G[R]
    def visitOr[R : Disjunction : ExtractBoolean](expr: Or[F, V, R, P]): G[R]
    def visitOutputWithinSet[R](expr: OutputWithinSet[F, V, R, P]): G[Boolean]
    def visitOutputWithinWindow[R](expr: OutputWithinWindow[F, V, R, P]): G[Boolean]
    def visitReturnInput(expr: ReturnInput[F, V, P]): G[F[V]]
    def visitSelectFromOutput[S, R](expr: SelectFromOutput[F, V, S, R, P]): G[R]
    def visitSubtractOutputs[R : Subtraction](expr: SubtractOutputs[F, V, R, P]): G[R]
    def visitUsingDefinitions[R](expr: UsingDefinitions[F, V, R, P]): G[R]
    def visitWhen[R](expr: When[F, V, R, P]): G[R]
    def visitWithFactsOfType[T, R](expr: WithFactsOfType[T, R, P]): G[R]
  }

  /*
   * Please keep the following expressions in the same order in Expr.scala and ExprResult.scala.
   * It makes it easier to find the mirror node.
   *
   * While alphabetical seems like the best way to organize these classes, in practice
   * certain nodes that are related to each other (like addition and subtraction) will have
   * very similar implementations and will likely need to be refactored in identical ways.
   *
   * The Visitor and its subclasses use alphabetical sorting because their definitions
   * are derivative and it is practically impossible to forget to update their implementation
   * without the compiler complaining.
   */

  // TODO: Should constants always act as their own evidence?
  /**
    * Uses the given value as the output of this expression and ignores the input.
    */
  final case class ConstOutput[R, P](
    value: R,
    evidence: Evidence,
    capture: CaptureP[Id, Unit, R, P],
  ) extends Expr[Id, Unit, R, P] {
    override def visit[G[_]](v: Visitor[Id, Unit, P, G]): G[R] = v.visitConstOutput(this)
  }

  /**
    * Returns the input [[F]] of [[V]] values as the output of this expression node.
    *
    * Essentially, this is the [[identity]] function represented as an [[Expr]].
    *
    * @note many of the other [[Expr]] nodes will take an `inputExpr`. This is useful for passing a selected value
    *       from the input into an expression operation, so that you can operate on it as the output parameter.
    *       This is useful to avoid needing to define separate nodes for the input / output sides of an expression.
    *       Instead, you just take an `inputExpr` and you can pass this node to move the input into the output.
    */
  final case class ReturnInput[F[_], V, P](capture: CaptureP[F, V, F[V], P]) extends Expr[F, V, F[V], P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[F[V]] = v.visitReturnInput(this)
  }

  // TODO: Abstract over the embeddable type?

//  /**
//    * The input of the expression is replaced with `Unit` and the output of the [[embeddedExpr]]
//    * is returned.
//    *
//    * This makes it possible to embed separate / reusable sub-expressions that don't depend on any contextual input
//    * into any sub-expression to substitute or defer the result to a separate computation.
//    *
//    * Typically, these nodes only exist to satisfy the type system and are skipped by visitors, however, the
//    * input to the [[Embed]] node can be captured via the input parameters and folded into the final result.
//    */
//  final case class EmbedUnit[F[_], V, R, P](
//    embeddedExpr: Expr[Id, Unit, R, P],
//    capture: CaptureP[F, V, R, P],
//  ) extends Expr[F, V, R, P] {
//    override def visit[G[_]](v: Visitor[F, V, P, G]): G[R] = v.visitEmbedUnit(this)
//  }

  /**
    * The input of the expression is replaced with the [[FactTable]] and the output of the [[embeddedExpr]]
    * is returned.
    *
    * This makes it possible to embed separate / reusable sub-expressions that don't depend on any contextual input
    * into any sub-expression to substitute or defer the result to a separate computation.
    *
    * Typically, these nodes only exist to satisfy the type system and are skipped by visitors, however, the
    * input to the [[Embed]] node can be captured via the input parameters and folded into the final result.
    */
  final case class Embed[F[_], V, U : ExtractFromAnyInput, R, P](
    embeddedExpr: Expr[Id, U, R, P],
    capture: CaptureP[F, V, R, P],
  ) extends Expr[F, V, R, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[R] = v.visitEmbed(this)
  }

  /**
    * A common top-level expression that defines an output without any input beyond the initial [[FactTable]].
    *
    * The given [[FactTypeSet]] is used to select the matching facts from the [[FactTable]] and provide them
    * as both the input value and [[ResultSet]] to the [[subExpr]].
    *
    * @note these expressions can be easily dropped into any place that accepts the [[subExpr]] output type `R`
    *       by wrapping the expression in an [[Embed]].
    */
  final case class WithFactsOfType[T, R, P](
    factTypeSet: FactTypeSet[T],
    subExpr: Expr[SortedSet, TypedFact[T], R, P],
    capture: CaptureP[Id, FactTable, R, P],
  ) extends Expr[Id, FactTable, R, P] {
    override def visit[G[_]](v: Visitor[Id, FactTable, P, G]): G[R] = v.visitWithFactsOfType(this)
  }

  /**
    * Same as [[Define]], but captures and hides common type parameters to avoid issues with wild-cards on
    * higher-kinded type parameters.
    *
    * @note I'm using a sealed trait instead of a type alias to avoid expressions from matching the type
    *       that do not supply the requisite metadata (provided by the only subclass [[Define]]).
    *
    * @see https://github.com/scala/bug/issues/8039 for more info
    */
  sealed trait Definition[P] extends Expr[Id, FactTable, FactSet, P]

  /**
    * Define or add another source for a [[FactType]].
    *
    * When used in conjunction with [[UsingDefinitions]], this node allows you to update the [[FactTable]]
    * used by sub-expressions so that they can now have access to the fact values returned by the [[definitionExpr]].
    */
  final case class Define[M[_] : Foldable, T, P](
    factType: FactType[T],
    definitionExpr: Expr[Id, FactTable, M[T], P],
    capture: CaptureP[Id, FactTable, FactSet, P],
  ) extends Definition[P] {
    override def visit[G[_]](v: Visitor[Id, FactTable, P, G]): G[FactSet] = v.visitDefine(this)
  }

  /**
    * Combines all [[Definition]]s and adds the resulting facts to the [[FactTable]], which is then provided
    * to the [[subExpr]] during evaluation.
    *
    * @note this is a way to sequence the evaluation of an expression to occur after the fact definitions it
    *       depends on have been evaluated and added to the [[FactTable]] as input. If your fact definitions
    *       require other facts to be defined for their computation, then you must wrap them with a
    *       [[UsingDefinitions]] expression node as well. Repeating this process until the directed acyclic
    *       graph of dependencies has been sequenced properly. Including the same fact definition twice is
    *       likely idempotent, however, there is no currently machinery to avoid unnecessary re-computation.
    */
  final case class UsingDefinitions[F[_], V, R, P](
    definitions: Vector[Definition[P]],
    subExpr: Expr[F, V, R, P],
    capture: CaptureP[F, V, R, P],
  ) extends Expr[F, V, R, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[R] = v.visitUsingDefinitions(this)
  }

  /**
    * Returns the [[Conjunction]] of all results of the given input expressions.
    *
    * All evidence is tracked using the logic defined in
    * [[com.rallyhealth.vapors.factfilter.evaluator.InterpretExprAsFunction.Output.conjunction]]
    *
    * @note this <i>does not</i> short-circuit.
    *       [[ResultSet]] for all input values are used in deciding the evidence for the result.
    *
    *       If you want to apply short-circuiting, you must implement it within the algebra using sorting,
    *       filtering, and limiting operators.
    */
  final case class And[F[_], V, R : Conjunction : ExtractBoolean, P](
    inputExprList: NonEmptyList[Expr[F, V, R, P]],
    capture: CaptureP[F, V, R, P],
  ) extends Expr[F, V, R, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[R] = v.visitAnd(this)
  }

  /**
    * Returns the [[Disjunction]] of all results of the given input expressions.
    *
    * All evidence is tracked using the logic defined in
    * [[com.rallyhealth.vapors.factfilter.evaluator.InterpretExprAsFunction.Output.disjunction]]
    *
    * @note this <i>does not</i> short-circuit.
    *       [[ResultSet]] for all input values are used in deciding the evidence for the result.
    *
    *       If you want to apply short-circuiting, you must implement it within the algebra using sorting,
    *       filtering, and limiting operators.
    */
  final case class Or[F[_], V, R : Disjunction : ExtractBoolean, P](
    inputExprList: NonEmptyList[Expr[F, V, R, P]],
    capture: CaptureP[F, V, R, P],
  ) extends Expr[F, V, R, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[R] = v.visitOr(this)
  }

  /**
    * Negates the output from the given [[inputExpr]], but keeps the same evidence.
    */
  final case class Not[F[_], V, R : Negation, P](
    inputExpr: Expr[F, V, R, P],
    capture: CaptureP[F, V, R, P],
  ) extends Expr[F, V, R, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[R] = v.visitNot(this)
  }

  /**
    * Conditional expression that depends on the output of a given boolean expression to determine whether to
    * evaluate the [[thenExpr]] or the [[elseExpr]].
    *
    * @note this expression <i>does</i> short-circuit
    */
  final case class When[F[_], V, R, P](
    condExpr: Expr[F, V, Boolean, P],
    thenExpr: Expr[F, V, R, P],
    elseExpr: Expr[F, V, R, P],
    capture: CaptureP[F, V, R, P],
  ) extends Expr[F, V, R, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[R] = v.visitWhen(this)
  }

  /**
    * Uses a [[NamedLens]] to select a value from the output of the given [[inputExpr]] and returns it as output.
    *
    * @note if you want to select from the input [[F]] of [[V]], you can pass [[ReturnInput]] as the [[inputExpr]].
    */
  final case class SelectFromOutput[F[_], V, S, R, P](
    inputExpr: Expr[F, V, S, P],
    lens: NamedLens[S, R],
    capture: CaptureP[F, V, R, P],
  ) extends Expr[F, V, R, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[R] = v.visitSelectFromOutput(this)
  }

  /**
    * [[Foldable.collectFoldSome]] every element in the output of the given [[inputExpr]].
    *
    * @note if you want to apply this to the input (i.e. `F[V]`), you can pass [[ReturnInput]] as the [[inputExpr]].
    */
  // TODO: Rename to CollectFoldSomeOutput?
  final case class CollectFromOutput[F[_], V, M[_] : Foldable, U, R : Monoid, P](
    inputExpr: Expr[F, V, M[U], P],
    collectExpr: Expr[Id, U, Option[R], P],
    capture: CaptureP[F, V, R, P],
  ) extends Expr[F, V, R, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[R] = v.visitCollectSomeOutput(this)
  }

  /**
    * [[FlatMap.flatMap]] every element in the output of the given [[inputExpr]].
    *
    * @note if you want to apply this to the input (i.e. `F[V]`), you can pass [[ReturnInput]] as the [[inputExpr]].
    */
  final case class FlatMapOutput[F[_], V, M[_] : Foldable : FlatMap, U, R, P](
    inputExpr: Expr[F, V, M[U], P],
    flatMapExpr: Expr[Id, U, M[R], P],
    capture: CaptureP[F, V, M[R], P],
  ) extends Expr[F, V, M[R], P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[M[R]] = v.visitFlatMapOutput(this)
  }

  /**
    * [[Functor.map]] every element in the output of the given [[inputExpr]].
    *
    * @note if you want to apply this to the input (i.e. `F[V]`), you can pass [[ReturnInput]] as the [[inputExpr]].
    */
  final case class MapOutput[F[_], V, M[_] : Foldable : Functor, U, R, P](
    inputExpr: Expr[F, V, M[U], P],
    mapExpr: Expr[Id, U, R, P],
    capture: CaptureP[F, V, M[R], P],
  ) extends Expr[F, V, M[R], P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[M[R]] = v.visitMapOutput(this)
  }

  /**
    * Returns `true` if at least one element in the output of the given [[inputExpr]] meets the given [[conditionExpr]].
    *
    * @note this <i>does not</i> short-circuit. [[ResultSet]] for all elements that match (or do not match) the
    *       [[conditionExpr]] will be retained as evidence of the whole result.
    * @note if you want to apply this to the input (i.e. `F[V]`), you can pass [[ReturnInput]] as the [[inputExpr]].
    * @see [[Foldable.exists]] for more details.
    */
  final case class ExistsInOutput[F[_], V, M[_] : Foldable, U, P](
    inputExpr: Expr[F, V, M[U], P],
    conditionExpr: Expr[Id, U, Boolean, P],
    capture: CaptureP[F, V, Boolean, P],
  ) extends Expr[F, V, Boolean, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[Boolean] = v.visitExistsInOutput(this)
  }

  /**
    * Adds the results of all expressions in [[inputExprList]] using the provided definition for [[Addition]].
    */
  final case class AddOutputs[F[_], V, R : Addition, P](
    inputExprList: NonEmptyList[Expr[F, V, R, P]],
    capture: CaptureP[F, V, R, P],
  ) extends Expr[F, V, R, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[R] = v.visitAddOutputs(this)
  }

  /**
    * Subtracts the results of all expressions in [[inputExprList]] using the provided definition for [[Subtraction]].
    *
    * @note the order of expressions matters for subtraction, so all subtraction is applied left-to-right.
    */
  final case class SubtractOutputs[F[_], V, R : Subtraction, P](
    inputExprList: NonEmptyList[Expr[F, V, R, P]],
    capture: CaptureP[F, V, R, P],
  ) extends Expr[F, V, R, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[R] = v.visitSubtractOutputs(this)
  }

  /**
    * Multiplies the results of all expressions in [[inputExprList]] using the provided definition for [[Multiplication]].
    */
  final case class MultiplyOutputs[F[_], V, R : Multiplication, P](
    inputExprList: NonEmptyList[Expr[F, V, R, P]],
    capture: CaptureP[F, V, R, P],
  ) extends Expr[F, V, R, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[R] = v.visitMultiplyOutputs(this)
  }

  /**
    * Multiplies the results of all expressions in [[inputExprList]] using the provided definition for [[Multiplication]].
    */
  final case class DivideOutputs[F[_], V, R : Division, P](
    inputExprList: NonEmptyList[Expr[F, V, R, P]],
    capture: CaptureP[F, V, R, P],
  ) extends Expr[F, V, R, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[R] = v.visitDivideOutputs(this)
  }

  /**
    * Returns the negative result value of [[inputExpr]] using the provided definition for [[Negative]].
    */
  final case class NegativeOutput[F[_], V, R : Negative, P](
    inputExpr: Expr[F, V, R, P],
    capture: CaptureP[F, V, R, P],
  ) extends Expr[F, V, R, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[R] = v.visitNegativeOutput(this)
  }

  /**
    * Checks if the result of the [[inputExpr]] is contained within the given set of [[accepted]] values.
    */
  final case class OutputWithinSet[F[_], V, R, P](
    inputExpr: Expr[F, V, R, P],
    accepted: Set[R],
    capture: CaptureP[F, V, Boolean, P],
  ) extends Expr[F, V, Boolean, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[Boolean] = v.visitOutputWithinSet(this)
  }

  /**
    * Checks if the result of the [[inputExpr]] is contained within the given [[window]].
    *
    * This is effectively how comparison checks are made.
    *
    * @see [[Window]] for more details.
    */
  final case class OutputWithinWindow[F[_], V, R, P](
    inputExpr: Expr[F, V, R, P],
    window: Window[R],
    capture: CaptureP[F, V, Boolean, P],
  ) extends Expr[F, V, Boolean, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[Boolean] = v.visitOutputWithinWindow(this)
  }
}
