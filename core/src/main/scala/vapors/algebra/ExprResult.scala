package com.rallyhealth

package vapors.algebra

import vapors.data.{FactSet, FactTable, TypedFact}
import vapors.interpreter.{ExprInput, ExprOutput}

import cats._
import cats.kernel.Monoid
import com.rallyhealth.vapors.data.{FactTable, TypedFact}
import shapeless.HList

import scala.collection.{BitSet, MapView}

/**
  * The result of evaluating an [[Expr]] with some given [[ExprInput]].
  *
  * You can fold over this result to see the input, output, value, and captured custom parameter at each node in the
  * original expression. Every node of this algebra matches to the [[Expr]] subclass with the same name.
  */
sealed trait ExprResult[V, R, P] {

  def context: ExprResult.Context[V, R, P]

  @inline final def input: ExprInput[V] = context.input

  @inline final def output: ExprOutput[R] = context.output

  @inline final def param: Eval[P] = context.param

  def convertOutputToInput: ExprInput[R] = input.withValue(output.value, output.evidence)

  def visit[G[_]](v: ExprResult.Visitor[V, P, G]): G[R]
}

object ExprResult {

  final case class Context[V, R, P](
    input: ExprInput[V],
    output: ExprOutput[R],
    param: Eval[P],
  )

  trait Visitor[V, P, G[_]] extends (ExprResult[V, *, P] ~> G) {
    // Please keep the following methods in alphabetical order
    override final def apply[R](fa: ExprResult[V, R, P]): G[R] = fa.visit(this)
    def visitAddOutputs[R](result: AddOutputs[V, R, P]): G[R]
    def visitAnd[R](result: And[V, R, P]): G[R]
    def visitCollectFromOutput[M[_] : Foldable, U, R : Monoid](result: CollectFromOutput[V, M, U, R, P]): G[R]
    def visitConcatOutput[M[_] : MonoidK, R](result: ConcatOutput[V, M, R, P]): G[M[R]]
    def visitConstOutput[R](result: ConstOutput[V, R, P]): G[R]
    def visitCustomFunction[A, R](result: CustomFunction[V, A, R, P]): G[R]
    def visitDeclare[M[_], T](result: Define[V, M, T, P]): G[FactSet]
    def visitDivideOutputs[R](result: DivideOutputs[V, R, P]): G[R]
    def visitEmbed[R](result: Embed[V, R, P]): G[R]
    def visitExistsInOutput[M[_] : Foldable, U](result: ExistsInOutput[V, M, U, P]): G[Boolean]
    def visitFilterOutput[M[_] : Foldable : FunctorFilter, R](result: FilterOutput[V, M, R, P]): G[M[R]]
    def visitFlatMapOutput[M[_], U, R](result: FlatMapOutput[V, M, U, R, P]): G[M[R]]
    def visitGroupOutput[M[_] : Foldable, U : Order, K](result: GroupOutput[V, M, U, K, P]): G[MapView[K, Seq[U]]]
    def visitMapOutput[M[_], U, R](result: MapOutput[V, M, U, R, P]): G[M[R]]
    def visitMultiplyOutputs[R](result: MultiplyOutputs[V, R, P]): G[R]
    def visitNegativeOutput[R](result: NegativeOutput[V, R, P]): G[R]
    def visitNot[R](result: Not[V, R, P]): G[R]
    def visitOr[R](result: Or[V, R, P]): G[R]
    def visitOutputIsEmpty[M[_] : Foldable, R](result: OutputIsEmpty[V, M, R, P]): G[Boolean]
    def visitOutputWithinSet[R](result: OutputWithinSet[V, R, P]): G[Boolean]
    def visitOutputWithinWindow[R](result: OutputWithinWindow[V, R, P]): G[Boolean]
    def visitFoldOutput[M[_] : Foldable, R : Monoid](result: FoldOutput[V, M, R, P]): G[R]
    def visitReturnInput(result: ReturnInput[V, P]): G[V]
    def visitSelectFromOutput[S, R](result: SelectFromOutput[V, S, R, P]): G[R]
    def visitSortOutput[M[_], R](result: SortOutput[V, M, R, P]): G[M[R]]
    def visitSubtractOutputs[R](result: SubtractOutputs[V, R, P]): G[R]
    def visitTakeFromOutput[M[_] : Traverse : TraverseFilter, R](result: TakeFromOutput[V, M, R, P]): G[M[R]]
    def visitUsingDefinitions[R](result: UsingDefinitions[V, R, P]): G[R]
    def visitWhen[R](result: When[V, R, P]): G[R]
    def visitWrapOutputHList[T <: HList, R](result: WrapOutputHList[V, T, R, P]): G[R]
    def visitWrapOutputSeq[R](result: WrapOutputSeq[V, R, P]): G[Seq[R]]
    def visitWithFactsOfType[T, R](result: WithFactsOfType[V, T, R, P]): G[R]
    def visitZipOutput[M[_] : Align : FunctorFilter, L <: HList, R](result: ZipOutput[V, M, L, R, P]): G[M[R]]
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

  final case class ConstOutput[V, R, P](
    expr: Expr.ConstOutput[V, R, P],
    context: Context[V, R, P],
  ) extends ExprResult[V, R, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[R] = v.visitConstOutput(this)
  }

  final case class ReturnInput[V, P](
    expr: Expr.ReturnInput[V, P],
    context: Context[V, V, P],
  ) extends ExprResult[V, V, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[V] = v.visitReturnInput(this)
  }

  final case class Embed[V, R, P](
    expr: Expr.Embed[V, R, P],
    context: Context[V, R, P],
    embeddedResult: ExprResult[FactTable, R, P],
  ) extends ExprResult[V, R, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[R] = v.visitEmbed(this)
  }

  final case class CustomFunction[V, A, R, P](
    expr: Expr.CustomFunction[V, A, R, P],
    context: Context[V, R, P],
    argResult: ExprResult[V, A, P],
  ) extends ExprResult[V, R, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[R] = v.visitCustomFunction(this)
  }

  final case class WithFactsOfType[V, T, R, P](
    expr: Expr.WithFactsOfType[T, R, P],
    context: Context[V, R, P],
    subResult: ExprResult[Seq[TypedFact[T]], R, P],
  ) extends ExprResult[V, R, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[R] = v.visitWithFactsOfType(this)
  }

  final case class Define[V, M[_], T, P](
    expr: Expr.Define[M, T, P],
    context: Context[V, FactSet, P],
    definitionResult: ExprResult[FactTable, M[T], P],
  ) extends ExprResult[V, FactSet, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[FactSet] = v.visitDeclare(this)
  }

  final case class UsingDefinitions[V, R, P](
    expr: Expr.UsingDefinitions[V, R, P],
    context: Context[V, R, P],
    subResult: ExprResult[V, R, P],
  ) extends ExprResult[V, R, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[R] = v.visitUsingDefinitions(this)
  }

  final case class And[V, R, P](
    expr: Expr.And[V, R, P],
    context: Context[V, R, P],
    subResultList: List[ExprResult[V, R, P]],
  ) extends ExprResult[V, R, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[R] = v.visitAnd(this)
  }

  final case class Or[V, R, P](
    expr: Expr.Or[V, R, P],
    context: Context[V, R, P],
    subResultList: List[ExprResult[V, R, P]],
  ) extends ExprResult[V, R, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[R] = v.visitOr(this)
  }

  final case class Not[V, R, P](
    expr: Expr.Not[V, R, P],
    context: Context[V, R, P],
    subResult: ExprResult[V, R, P],
  ) extends ExprResult[V, R, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[R] = v.visitNot(this)
  }

  final case class When[V, R, P](
    expr: Expr.When[V, R, P],
    context: Context[V, R, P],
    matchedBranch: Option[ConditionBranch[V, R, P]],
    subResult: ExprResult[V, R, P],
  ) extends ExprResult[V, R, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[R] = v.visitWhen(this)
    val thenExpr: Expr[V, R, P] = matchedBranch.map(_.thenExpr).getOrElse(expr.defaultExpr)
  }

  final case class SelectFromOutput[V, S, R, P](
    expr: Expr.SelectFromOutput[V, S, R, P],
    context: Context[V, R, P],
    inputResult: ExprResult[V, S, P],
  ) extends ExprResult[V, R, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[R] = v.visitSelectFromOutput(this)
  }

  final case class FilterOutput[V, M[_] : Foldable : FunctorFilter, R, P](
    expr: Expr.FilterOutput[V, M, R, P],
    context: Context[V, M[R], P],
    inputResult: ExprResult[V, M[R], P],
    condResultList: List[ExprResult[R, Boolean, P]],
  ) extends ExprResult[V, M[R], P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[M[R]] = v.visitFilterOutput(this)
  }

  final case class CollectFromOutput[V, M[_] : Foldable, U, R : Monoid, P](
    expr: Expr.CollectFromOutput[V, M, U, R, P],
    context: Context[V, R, P],
    inputResult: ExprResult[V, M[U], P],
  ) extends ExprResult[V, R, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[R] = v.visitCollectFromOutput(this)
  }

  final case class FlatMapOutput[V, M[_], U, R, P](
    expr: Expr.FlatMapOutput[V, M, U, R, P],
    context: Context[V, M[R], P],
    inputResult: ExprResult[V, M[U], P],
    subResultList: List[ExprResult[U, M[R], P]],
  ) extends ExprResult[V, M[R], P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[M[R]] = v.visitFlatMapOutput(this)
  }

  final case class MapOutput[V, M[_], U, R, P](
    expr: Expr.MapOutput[V, M, U, R, P],
    context: Context[V, M[R], P],
    inputResult: ExprResult[V, M[U], P],
    subResultList: List[ExprResult[U, R, P]],
  ) extends ExprResult[V, M[R], P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[M[R]] = v.visitMapOutput(this)
  }

  final case class GroupOutput[V, M[_] : Foldable, U : Order, K, P](
    expr: Expr.GroupOutput[V, M, U, K, P],
    context: Context[V, MapView[K, Seq[U]], P],
    inputResult: ExprResult[V, M[U], P],
  ) extends ExprResult[V, MapView[K, Seq[U]], P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[MapView[K, Seq[U]]] = v.visitGroupOutput(this)
  }

  final case class SortOutput[V, M[_], R, P](
    expr: Expr.SortOutput[V, M, R, P],
    context: Context[V, M[R], P],
    inputResult: ExprResult[V, M[R], P],
  ) extends ExprResult[V, M[R], P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[M[R]] = v.visitSortOutput(this)
  }

  final case class ConcatOutput[V, M[_] : MonoidK, R, P](
    expr: Expr.ConcatOutput[V, M, R, P],
    context: Context[V, M[R], P],
    inputResultList: Seq[ExprResult[V, M[R], P]],
  ) extends ExprResult[V, M[R], P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[M[R]] = v.visitConcatOutput(this)
  }

  final case class FoldOutput[V, M[_] : Foldable, R : Monoid, P](
    expr: Expr.FoldOutput[V, M, R, P],
    context: Context[V, R, P],
    inputResult: ExprResult[V, M[R], P],
  ) extends ExprResult[V, R, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[R] = v.visitFoldOutput(this)
  }

  final case class OutputIsEmpty[V, M[_] : Foldable, R, P](
    expr: Expr.OutputIsEmpty[V, M, R, P],
    context: Context[V, Boolean, P],
    inputResult: ExprResult[V, M[R], P],
  ) extends ExprResult[V, Boolean, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[Boolean] = v.visitOutputIsEmpty(this)
  }

  final case class TakeFromOutput[V, M[_] : Traverse : TraverseFilter, R, P](
    expr: Expr.TakeFromOutput[V, M, R, P],
    context: Context[V, M[R], P],
    inputResult: ExprResult[V, M[R], P],
  ) extends ExprResult[V, M[R], P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[M[R]] = v.visitTakeFromOutput(this)
  }

  final case class ExistsInOutput[V, M[_] : Foldable, U, P](
    expr: Expr.ExistsInOutput[V, M, U, P],
    context: Context[V, Boolean, P],
    inputResult: ExprResult[V, M[U], P],
    conditionResultList: List[ExprResult[U, Boolean, P]],
    matchedIndexes: BitSet,
  ) extends ExprResult[V, Boolean, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[Boolean] = v.visitExistsInOutput(this)
  }

  final case class AddOutputs[V, R, P](
    expr: Expr.AddOutputs[V, R, P],
    context: Context[V, R, P],
    subResultList: List[ExprResult[V, R, P]],
  ) extends ExprResult[V, R, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[R] = v.visitAddOutputs(this)
  }

  final case class SubtractOutputs[V, R, P](
    expr: Expr.SubtractOutputs[V, R, P],
    context: Context[V, R, P],
    subResultList: List[ExprResult[V, R, P]],
  ) extends ExprResult[V, R, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[R] = v.visitSubtractOutputs(this)
  }

  final case class MultiplyOutputs[V, R, P](
    expr: Expr.MultiplyOutputs[V, R, P],
    context: Context[V, R, P],
    subResultList: List[ExprResult[V, R, P]],
  ) extends ExprResult[V, R, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[R] = v.visitMultiplyOutputs(this)
  }

  final case class DivideOutputs[V, R, P](
    expr: Expr.DivideOutputs[V, R, P],
    context: Context[V, R, P],
    subResultList: List[ExprResult[V, R, P]],
  ) extends ExprResult[V, R, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[R] = v.visitDivideOutputs(this)
  }

  final case class WrapOutputSeq[V, R, P](
    expr: Expr.WrapOutputSeq[V, R, P],
    context: Context[V, Seq[R], P],
    inputResultList: Seq[ExprResult[V, R, P]],
  ) extends ExprResult[V, Seq[R], P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[Seq[R]] = v.visitWrapOutputSeq(this)
  }

  final case class WrapOutputHList[V, T <: HList, R, P](
    expr: Expr.WrapOutputHList[V, T, R, P],
    context: Context[V, R, P],
  ) extends ExprResult[V, R, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[R] = v.visitWrapOutputHList(this)
  }

  final case class ZipOutput[V, M[_] : Align : FunctorFilter, L <: HList, R, P](
    expr: Expr.ZipOutput[V, M, L, R, P],
    context: Context[V, M[R], P],
  ) extends ExprResult[V, M[R], P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[M[R]] = v.visitZipOutput(this)
  }

  final case class NegativeOutput[V, R, P](
    expr: Expr.NegativeOutput[V, R, P],
    context: Context[V, R, P],
    inputResult: ExprResult[V, R, P],
  ) extends ExprResult[V, R, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[R] = v.visitNegativeOutput(this)
  }

  final case class OutputWithinSet[V, R, P](
    expr: Expr.OutputWithinSet[V, R, P],
    context: Context[V, Boolean, P],
    inputResult: ExprResult[V, R, P],
  ) extends ExprResult[V, Boolean, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[Boolean] = v.visitOutputWithinSet(this)
  }

  final case class OutputWithinWindow[V, R, P](
    expr: Expr.OutputWithinWindow[V, R, P],
    context: Context[V, Boolean, P],
    inputResult: ExprResult[V, R, P],
  ) extends ExprResult[V, Boolean, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[Boolean] = v.visitOutputWithinWindow(this)
  }

}
