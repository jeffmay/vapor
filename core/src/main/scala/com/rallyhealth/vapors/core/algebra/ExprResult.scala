package com.rallyhealth.vapors.core.algebra

import cats.kernel.Monoid
import cats.{~>, Eval, Foldable, Id}
import com.rallyhealth.vapors.factfilter.data.{ExtractFromAnyInput, FactSet, FactTable, TypedFact}
import com.rallyhealth.vapors.factfilter.evaluator.InterpretExprAsFunction.{Input, Output}

import scala.collection.BitSet
import scala.collection.immutable.SortedSet

/**
  * The result of evaluating an [[Expr]] with some given [[Input]].
  *
  * You can fold over this result to see the input, output, value, and captured custom parameter at each node in the
  * original expression. Every node of this algebra matches to the [[Expr]] subclass with the same name.
  */
sealed trait ExprResult[F[_], V, R, P] {

  def context: ExprResult.Context[F, V, R, P]

  @inline final def input: Input[F, V] = context.input

  @inline final def output: Output[R] = context.output

  @inline final def param: Eval[P] = context.param

  def visit[G[_]](v: ExprResult.Visitor[F, V, P, G]): G[R]
}

object ExprResult {

  final case class Context[F[_], V, R, P](
    input: Input[F, V],
    output: Output[R],
    param: Eval[P],
  )

  trait Visitor[F[_], V, P, G[_]] extends (ExprResult[F, V, *, P] ~> G) {
    // Please keep the following methods in alphabetical order
    override final def apply[R](fa: ExprResult[F, V, R, P]): G[R] = fa.visit(this)
    def visitAddOutputs[R](result: AddOutputs[F, V, R, P]): G[R]
    def visitAnd[R](result: And[F, V, R, P]): G[R]
    def visitCollectFromOutput[M[_] : Foldable, U, R : Monoid](result: CollectFromOutput[F, V, M, U, R, P]): G[R]
    def visitConstOutput[R](result: ConstOutput[F, V, R, P]): G[R]
    def visitDeclare[M[_], T](result: Define[F, V, M, T, P]): G[FactSet]
    def visitDivideOutputs[R](result: DivideOutputs[F, V, R, P]): G[R]
    def visitEmbed[U : ExtractFromAnyInput, R](result: Embed[F, V, U, R, P]): G[R]
//    def visitEmbedUnit[R](result: EmbedUnit[F, V, R, P]): G[R]
    def visitExistsInOutput[M[_] : Foldable, U](result: ExistsInOutput[F, V, M, U, P]): G[Boolean]
    def visitFlatMapOutput[M[_], U, R](result: FlatMapOutput[F, V, M, U, R, P]): G[M[R]]
    def visitMapOutput[M[_], U, R](result: MapOutput[F, V, M, U, R, P]): G[M[R]]
    def visitMultiplyOutputs[R](result: MultiplyOutputs[F, V, R, P]): G[R]
    def visitNegativeOutput[R](result: NegativeOutput[F, V, R, P]): G[R]
    def visitNot[R](result: Not[F, V, R, P]): G[R]
    def visitOr[R](result: Or[F, V, R, P]): G[R]
    def visitOutputWithinSet[R](result: OutputWithinSet[F, V, R, P]): G[Boolean]
    def visitOutputWithinWindow[R](result: OutputWithinWindow[F, V, R, P]): G[Boolean]
    def visitReturnInput(result: ReturnInput[F, V, P]): G[F[V]]
    def visitSelectFromOutput[S, R](result: SelectFromOutput[F, V, S, R, P]): G[R]
    def visitSubtractOutputs[R](result: SubtractOutputs[F, V, R, P]): G[R]
    def visitUsingDefinitions[R](result: UsingDefinitions[F, V, R, P]): G[R]
    def visitWhen[R](result: When[F, V, R, P]): G[R]
    def visitWithFactsOfType[T, R](result: WithFactsOfType[F, V, T, R, P]): G[R]
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

  final case class ConstOutput[F[_], V, R, P](
    expr: Expr.ConstOutput[R, P],
    context: Context[F, V, R, P],
  ) extends ExprResult[F, V, R, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[R] = v.visitConstOutput(this)
  }

  final case class ReturnInput[F[_], V, P](
    expr: Expr.ReturnInput[F, V, P],
    context: Context[F, V, F[V], P],
  ) extends ExprResult[F, V, F[V], P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[F[V]] = v.visitReturnInput(this)
  }

  final case class Embed[F[_], V, U : ExtractFromAnyInput, R, P](
    expr: Expr.Embed[F, V, U, R, P],
    context: Context[F, V, R, P],
    embeddedResult: ExprResult[Id, U, R, P],
  ) extends ExprResult[F, V, R, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[R] = v.visitEmbed(this)
  }

//  final case class EmbedUnit[F[_], V, R, P](
//    expr: Expr.EmbedUnit[F, V, R, P],
//    context: Context[F, V, R, P],
//    embeddedResult: ExprResult[Id, Unit, R, P],
//  ) extends ExprResult[F, V, R, P] {
//    override def visit[G[_]](v: Visitor[F, V, P, G]): G[R] = v.visitEmbedUnit(this)
//  }

  final case class WithFactsOfType[F[_], V, T, R, P](
    expr: Expr.WithFactsOfType[T, R, P],
    context: Context[F, V, R, P],
    subResult: ExprResult[SortedSet, TypedFact[T], R, P],
  ) extends ExprResult[F, V, R, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[R] = v.visitWithFactsOfType(this)
  }

  final case class Define[F[_], V, M[_], T, P](
    expr: Expr.Define[M, T, P],
    context: Context[F, V, FactSet, P],
    definitionResult: ExprResult[Id, FactTable, M[T], P],
  ) extends ExprResult[F, V, FactSet, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[FactSet] = v.visitDeclare(this)
  }

  final case class UsingDefinitions[F[_], V, R, P](
    expr: Expr.UsingDefinitions[F, V, R, P],
    context: Context[F, V, R, P],
    subResult: ExprResult[F, V, R, P],
  ) extends ExprResult[F, V, R, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[R] = v.visitUsingDefinitions(this)
  }

  final case class And[F[_], V, R, P](
    expr: Expr.And[F, V, R, P],
    context: Context[F, V, R, P],
    subResultList: List[ExprResult[F, V, R, P]],
  ) extends ExprResult[F, V, R, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[R] = v.visitAnd(this)
  }

  final case class Or[F[_], V, R, P](
    expr: Expr.Or[F, V, R, P],
    context: Context[F, V, R, P],
    subResultList: List[ExprResult[F, V, R, P]],
  ) extends ExprResult[F, V, R, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[R] = v.visitOr(this)
  }

  final case class Not[F[_], V, R, P](
    expr: Expr.Not[F, V, R, P],
    context: Context[F, V, R, P],
    subResult: ExprResult[F, V, R, P],
  ) extends ExprResult[F, V, R, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[R] = v.visitNot(this)
  }

  final case class When[F[_], V, R, P](
    expr: Expr.When[F, V, R, P],
    context: Context[F, V, R, P],
    conditionResult: ExprResult[F, V, Boolean, P],
    subResult: ExprResult[F, V, R, P],
  ) extends ExprResult[F, V, R, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[R] = v.visitWhen(this)
    @inline def conditionMet: Boolean = conditionResult.output.value
  }

  final case class SelectFromOutput[F[_], V, S, R, P](
    expr: Expr.SelectFromOutput[F, V, S, R, P],
    context: Context[F, V, R, P],
    inputResult: ExprResult[F, V, S, P],
  ) extends ExprResult[F, V, R, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[R] = v.visitSelectFromOutput(this)
  }

  final case class CollectFromOutput[F[_], V, M[_] : Foldable, U, R : Monoid, P](
    expr: Expr.CollectFromOutput[F, V, M, U, R, P],
    context: Context[F, V, R, P],
    inputResult: ExprResult[F, V, M[U], P],
  ) extends ExprResult[F, V, R, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[R] = v.visitCollectFromOutput(this)
  }

  final case class FlatMapOutput[F[_], V, M[_], U, R, P](
    expr: Expr.FlatMapOutput[F, V, M, U, R, P],
    context: Context[F, V, M[R], P],
    inputResult: ExprResult[F, V, M[U], P],
    subResultList: List[ExprResult[Id, U, M[R], P]],
  ) extends ExprResult[F, V, M[R], P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[M[R]] = v.visitFlatMapOutput(this)
  }

  final case class MapOutput[F[_], V, M[_], U, R, P](
    expr: Expr.MapOutput[F, V, M, U, R, P],
    context: Context[F, V, M[R], P],
    inputResult: ExprResult[F, V, M[U], P],
    subResultList: List[ExprResult[Id, U, R, P]],
  ) extends ExprResult[F, V, M[R], P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[M[R]] = v.visitMapOutput(this)
  }

  final case class ExistsInOutput[F[_], V, M[_] : Foldable, U, P](
    expr: Expr.ExistsInOutput[F, V, M, U, P],
    context: Context[F, V, Boolean, P],
    inputResult: ExprResult[F, V, M[U], P],
    conditionResultList: List[ExprResult[Id, U, Boolean, P]],
    matchedIndexes: BitSet,
  ) extends ExprResult[F, V, Boolean, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[Boolean] = v.visitExistsInOutput(this)
  }

  final case class AddOutputs[F[_], V, R, P](
    expr: Expr.AddOutputs[F, V, R, P],
    context: Context[F, V, R, P],
    subResultList: List[ExprResult[F, V, R, P]],
  ) extends ExprResult[F, V, R, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[R] = v.visitAddOutputs(this)
  }

  final case class SubtractOutputs[F[_], V, R, P](
    expr: Expr.SubtractOutputs[F, V, R, P],
    context: Context[F, V, R, P],
    subResultList: List[ExprResult[F, V, R, P]],
  ) extends ExprResult[F, V, R, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[R] = v.visitSubtractOutputs(this)
  }

  final case class MultiplyOutputs[F[_], V, R, P](
    expr: Expr.MultiplyOutputs[F, V, R, P],
    context: Context[F, V, R, P],
    subResultList: List[ExprResult[F, V, R, P]],
  ) extends ExprResult[F, V, R, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[R] = v.visitMultiplyOutputs(this)
  }

  final case class DivideOutputs[F[_], V, R, P](
    expr: Expr.DivideOutputs[F, V, R, P],
    context: Context[F, V, R, P],
    subResultList: List[ExprResult[F, V, R, P]],
  ) extends ExprResult[F, V, R, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[R] = v.visitDivideOutputs(this)
  }

  final case class NegativeOutput[F[_], V, R, P](
    expr: Expr.NegativeOutput[F, V, R, P],
    context: Context[F, V, R, P],
    inputResult: ExprResult[F, V, R, P],
  ) extends ExprResult[F, V, R, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[R] = v.visitNegativeOutput(this)
  }

  final case class OutputWithinSet[F[_], V, R, P](
    expr: Expr.OutputWithinSet[F, V, R, P],
    context: Context[F, V, Boolean, P],
    inputResult: ExprResult[F, V, R, P],
  ) extends ExprResult[F, V, Boolean, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[Boolean] = v.visitOutputWithinSet(this)
  }

  final case class OutputWithinWindow[F[_], V, R, P](
    expr: Expr.OutputWithinWindow[F, V, R, P],
    context: Context[F, V, Boolean, P],
    inputResult: ExprResult[F, V, R, P],
  ) extends ExprResult[F, V, Boolean, P] {
    override def visit[G[_]](v: Visitor[F, V, P, G]): G[Boolean] = v.visitOutputWithinWindow(this)
  }

}
