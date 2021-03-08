package com.rallyhealth.vapors.core.algebra

import cats._
import cats.data.NonEmptyList
import com.rallyhealth.vapors.core.data._
import com.rallyhealth.vapors.core.interpreter.{ExprOutput, InterpretExprAsResultFn}
import com.rallyhealth.vapors.core.lens.NamedLens
import com.rallyhealth.vapors.core.logic.{Conjunction, Disjunction, Negation}
import com.rallyhealth.vapors.core.math.{Addition, Negative, Subtraction}
import shapeless.HList

import scala.collection.MapView

/**
  * The core expression algebra.
  *
  * In essence, you can view this as a function from F[V] => R with a custom parameter that you can capture at each
  * point in the expression using a [[CaptureP]].
  *
  * This expression is folded over to produce serialized versions of the expression as well as the evaluator function.
  *
  * @see [[InterpretExprAsResultFn]]
  *
  * F = Foldable type constructor
  * V = Value type
  * F[V] = the input value type
  * R = Return type
  * P = Captured param
  */
sealed abstract class Expr[V, R, P] {

  def visit[G[_]](v: Expr.Visitor[V, P, G]): G[R]

  def capture: CaptureP[V, R, P]
}

object Expr {

  // TODO: Is it possible to define the visitor as having 2 free type parameters?
  //       trait FunctionK2[F[_, _], G[_, _]] {
  //         def apply[A, B](f: F[A, B]): G[A, B]
  //       }
  //       Visitor[P, G[_, _]] extends Function2K[Expr[*, *, P], G]] {
  //         def apply[V, R](fa: Expr[V, R, P]): G[V, R]
  //       }

  import cats.{~>, Id}

  trait Visitor[V, P, G[_]] extends (Expr[V, *, P] ~> G) {
    // Please keep the following methods in alphabetical order
    override final def apply[R](fa: Expr[V, R, P]): G[R] = fa.visit(this)
    def visitAddOutputs[R : Addition](expr: AddOutputs[V, R, P]): G[R]
    def visitAnd[R : Conjunction : ExtractBoolean](expr: And[V, R, P]): G[R]
    def visitCollectSomeOutput[M[_] : Foldable, U, R : Monoid](expr: CollectFromOutput[V, M, U, R, P]): G[R]
    def visitConcatOutput[M[_] : MonoidK, R](expr: ConcatOutput[V, M, R, P]): G[M[R]]
    def visitConstOutput[R](expr: ConstOutput[V, R, P]): G[R]
    def visitDefine[M[_] : Foldable, T](expr: Define[M, T, P]): G[FactSet]
    def visitEmbed[R](expr: Embed[V, R, P]): G[R]
    def visitExistsInOutput[M[_] : Foldable, U](expr: ExistsInOutput[V, M, U, P]): G[Boolean]
    def visitFilterOutput[M[_] : Foldable : FunctorFilter, R](expr: FilterOutput[V, M, R, P]): G[M[R]]
    def visitFlatMapOutput[M[_] : Foldable : FlatMap, U, X](expr: FlatMapOutput[V, M, U, X, P]): G[M[X]]
    def visitGroupOutput[M[_] : Foldable, U : Order, K](expr: GroupOutput[V, M, U, K, P]): G[MapView[K, Seq[U]]]
    def visitMapOutput[M[_] : Foldable : Functor, U, R](expr: MapOutput[V, M, U, R, P]): G[M[R]]
    def visitNegativeOutput[R : Negative](expr: NegativeOutput[V, R, P]): G[R]
    def visitNot[R : Negation](expr: Not[V, R, P]): G[R]
    def visitOr[R : Disjunction : ExtractBoolean](expr: Or[V, R, P]): G[R]
    def visitOutputIsEmpty[M[_] : Foldable, R](expr: OutputIsEmpty[V, M, R, P]): G[Boolean]
    def visitOutputWithinSet[R](expr: OutputWithinSet[V, R, P]): G[Boolean]
    def visitOutputWithinWindow[R](expr: OutputWithinWindow[V, R, P]): G[Boolean]
    def visitReturnInput(expr: ReturnInput[V, P]): G[V]
    def visitSelectFromOutput[S, R](expr: SelectFromOutput[V, S, R, P]): G[R]
    def visitSortOutput[M[_], R](expr: SortOutput[V, M, R, P]): G[M[R]]
    def visitSubtractOutputs[R : Subtraction](expr: SubtractOutputs[V, R, P]): G[R]
    def visitTakeFromOutput[M[_] : Traverse : TraverseFilter, R](expr: TakeFromOutput[V, M, R, P]): G[M[R]]
    def visitUsingDefinitions[R](expr: UsingDefinitions[V, R, P]): G[R]
    def visitWhen[R](expr: When[V, R, P]): G[R]
    def visitWrapOutputHList[T <: HList, R](expr: WrapOutputHList[V, T, R, P]): G[R]
    def visitWrapOutputSeq[R](expr: WrapOutputSeq[V, R, P]): G[Seq[R]]
    def visitWithFactsOfType[T, R](expr: WithFactsOfType[T, R, P]): G[R]
    def visitZipOutput[M[_] : Align : FunctorFilter, L <: HList, R](expr: ZipOutput[V, M, L, R, P]): G[M[R]]

    def visitZipWithDefaults[M[_] : Align : Functor, RL <: HList, IEL <: ExprHList[V, P]](
      expr: ZipWithDefaults[V, M, RL, IEL, P],
    ): G[M[RL]]
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
  final case class ConstOutput[V, R, P](
    value: R,
    evidence: Evidence,
    capture: CaptureP[V, R, P],
  ) extends Expr[V, R, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[R] = v.visitConstOutput(this)
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
  final case class ReturnInput[V, P](capture: CaptureP[V, V, P]) extends Expr[V, V, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[V] = v.visitReturnInput(this)
  }

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
  final case class Embed[V, R, P](
    embeddedExpr: Expr[FactTable, R, P],
    capture: CaptureP[V, R, P],
  ) extends Expr[V, R, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[R] = v.visitEmbed(this)
  }

  /**
    * A common top-level expression that defines an output without any input beyond the initial [[FactTable]].
    *
    * The given [[FactTypeSet]] is used to select the matching facts from the [[FactTable]] and provide them
    * as both the input value and [[Evidence]] to the [[subExpr]].
    *
    * @note these expressions can be easily dropped into any place that accepts the [[subExpr]] output type `R`
    *       by wrapping the expression in an [[Embed]].
    */
  final case class WithFactsOfType[T, R, P](
    factTypeSet: FactTypeSet[T],
    subExpr: Expr[Seq[TypedFact[T]], R, P],
    capture: CaptureP[FactTable, R, P],
  ) extends Expr[FactTable, R, P] {
    override def visit[G[_]](v: Visitor[FactTable, P, G]): G[R] = v.visitWithFactsOfType(this)
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
  sealed trait Definition[P] extends Expr[FactTable, FactSet, P]

  /**
    * Define or add another source for a [[FactType]].
    *
    * When used in conjunction with [[UsingDefinitions]], this node allows you to update the [[FactTable]]
    * used by sub-expressions so that they can now have access to the fact values returned by the [[definitionExpr]].
    */
  final case class Define[M[_] : Foldable, T, P](
    factType: FactType[T],
    definitionExpr: Expr[FactTable, M[T], P],
    capture: CaptureP[FactTable, FactSet, P],
  ) extends Definition[P] {
    override def visit[G[_]](v: Visitor[FactTable, P, G]): G[FactSet] = v.visitDefine(this)
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
  final case class UsingDefinitions[V, R, P](
    definitions: Vector[Definition[P]],
    subExpr: Expr[V, R, P],
    capture: CaptureP[V, R, P],
  ) extends Expr[V, R, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[R] = v.visitUsingDefinitions(this)
  }

  /**
    * Returns the [[Conjunction]] of all results of the given input expressions.
    *
    * All evidence is tracked using the logic defined in [[ExprOutput.conjunction]]
    *
    * @note this <i>does not</i> short-circuit.
    *       [[Evidence]] for all input values are used in deciding the evidence for the result.
    *
    *       If you want to apply short-circuiting, you must implement it within the algebra using sorting,
    *       filtering, and limiting operators.
    */
  final case class And[V, R : Conjunction : ExtractBoolean, P](
    inputExprList: NonEmptyList[Expr[V, R, P]],
    capture: CaptureP[V, R, P],
  ) extends Expr[V, R, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[R] = v.visitAnd(this)
  }

  /**
    * Returns the [[Disjunction]] of all results of the given input expressions.
    *
    * All evidence is tracked using the logic defined in [[ExprOutput.disjunction]]
    *
    * @note this <i>does not</i> short-circuit.
    *       [[Evidence]] for all input values are used in deciding the evidence for the result.
    *
    *       If you want to apply short-circuiting, you must implement it within the algebra using sorting,
    *       filtering, and limiting operators.
    */
  final case class Or[V, R : Disjunction : ExtractBoolean, P](
    inputExprList: NonEmptyList[Expr[V, R, P]],
    capture: CaptureP[V, R, P],
  ) extends Expr[V, R, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[R] = v.visitOr(this)
  }

  /**
    * Negates the output from the given [[inputExpr]], but keeps the same evidence.
    */
  final case class Not[V, R : Negation, P](
    inputExpr: Expr[V, R, P],
    capture: CaptureP[V, R, P],
  ) extends Expr[V, R, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[R] = v.visitNot(this)
  }

  /**
    * Conditional expression that depends on the output of a given boolean expression to determine whether to
    * evaluate the [[conditionBranches]] or the [[defaultExpr]].
    *
    * @note this expression <i>does</i> short-circuit on the first branch whose condition is met
    */
  final case class When[V, R, P](
    conditionBranches: NonEmptyList[ConditionBranch[V, R, P]],
    defaultExpr: Expr[V, R, P],
    capture: CaptureP[V, R, P],
  ) extends Expr[V, R, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[R] = v.visitWhen(this)
  }

  /**
    * Uses a [[NamedLens]] to select a value from the output of the given [[inputExpr]] and returns it as output.
    *
    * @note if you want to select from the input [[F]] of [[V]], you can pass [[ReturnInput]] as the [[inputExpr]].
    */
  final case class SelectFromOutput[V, S, R, P](
    inputExpr: Expr[V, S, P],
    lens: NamedLens[S, R],
    capture: CaptureP[V, R, P],
  ) extends Expr[V, R, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[R] = v.visitSelectFromOutput(this)
  }

  final case class FilterOutput[V, M[_] : Foldable : FunctorFilter, R, P](
    inputExpr: Expr[V, M[R], P],
    condExpr: Expr[R, Boolean, P],
    capture: CaptureP[V, M[R], P],
  ) extends Expr[V, M[R], P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[M[R]] = v.visitFilterOutput(this)
  }

  /**
    * [[Foldable.collectFoldSome]] every element in the output of the given [[inputExpr]].
    *
    * @note if you want to apply this to the input (i.e. `F[V]`), you can pass [[ReturnInput]] as the [[inputExpr]].
    */
  // TODO: Rename to CollectFoldSomeOutput?
  final case class CollectFromOutput[V, M[_] : Foldable, U, R : Monoid, P](
    inputExpr: Expr[V, M[U], P],
    collectExpr: Expr[U, Option[R], P],
    capture: CaptureP[V, R, P],
  ) extends Expr[V, R, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[R] = v.visitCollectSomeOutput(this)
  }

  /**
    * [[FlatMap.flatMap]] every element in the output of the given [[inputExpr]].
    *
    * @note if you want to apply this to the input (i.e. `F[V]`), you can pass [[ReturnInput]] as the [[inputExpr]].
    */
  final case class FlatMapOutput[V, M[_] : Foldable : FlatMap, U, R, P](
    inputExpr: Expr[V, M[U], P],
    flatMapExpr: Expr[U, M[R], P],
    capture: CaptureP[V, M[R], P],
  ) extends Expr[V, M[R], P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[M[R]] = v.visitFlatMapOutput(this)
  }

  /**
    * [[Functor.map]] every element in the output of the given [[inputExpr]].
    *
    * @note if you want to apply this to the input (i.e. `F[V]`), you can pass [[ReturnInput]] as the [[inputExpr]].
    */
  final case class MapOutput[V, M[_] : Foldable : Functor, U, R, P](
    inputExpr: Expr[V, M[U], P],
    mapExpr: Expr[U, R, P],
    capture: CaptureP[V, M[R], P],
  ) extends Expr[V, M[R], P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[M[R]] = v.visitMapOutput(this)
  }

  /**
    * Group the output of a given expression into a Map of some hashable key type.
    *
    * @note this uses the default (_: K).## method for hashing because it uses the standard Scala Map collection.
    */
  final case class GroupOutput[V, M[_] : Foldable, U : Order, K, P](
    inputExpr: Expr[V, M[U], P],
    groupByLens: NamedLens[U, K],
    capture: CaptureP[V, MapView[K, Seq[U]], P],
  ) extends Expr[V, MapView[K, Seq[U]], P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[MapView[K, Seq[U]]] = v.visitGroupOutput(this)
  }

  /**
    * Sort the elements in the output of the given [[inputExpr]] using the provided [[ExprSorter]].
    *
    * @note if you want to apply this to the input (i.e. `F[V]`), you can pass [[ReturnInput]] as the [[inputExpr]].
    */
  final case class SortOutput[V, M[_], R, P](
    inputExpr: Expr[V, M[R], P],
    sorter: ExprSorter[M, R],
    capture: CaptureP[V, M[R], P],
  ) extends Expr[V, M[R], P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[M[R]] = v.visitSortOutput(this)
  }

  /**
    * Concatenate the outputs of all the given expressions into a single expression of the concatenated monoid.
    *
    * @note if you want the output to be evaluated only as needed, then you should use a LazyList for the
    *       [[inputExprList]] param sequence type.
    */
  final case class ConcatOutput[V, M[_] : MonoidK, R, P](
    inputExprList: Seq[Expr[V, M[R], P]],
    capture: CaptureP[V, M[R], P],
  ) extends Expr[V, M[R], P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[M[R]] = v.visitConcatOutput(this)
  }

  /**
    * Wrap a sequence of expressions into a single expression of a lazy sequence that evaluates only the
    * expressions needed to produce the values used in subsequent expression nodes.
    */
  final case class WrapOutputSeq[V, R, P](
    inputExprList: Seq[Expr[V, R, P]],
    capture: CaptureP[V, Seq[R], P],
  ) extends Expr[V, Seq[R], P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[Seq[R]] = v.visitWrapOutputSeq(this)
  }

  /**
    * Convert a non-empty HList of expressions into an expression of HList, then map a [[converter]] function.
    *
    * This is very similar to [[ZipOutput]], except that for concrete types, there is no possibility of having
    * anything other than a single instance of the expected type, so you don't need any type-classes for the
    * return type.
    */
  final case class WrapOutputHList[V, L <: HList, R, P](
    inputExprHList: NonEmptyExprHList[V, Id, L, P],
    converter: ExprConverter[L, R],
    capture: CaptureP[V, R, P],
  ) extends Expr[V, R, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[R] = v.visitWrapOutputHList(this)
  }

  /**
    * Apply a given [[converter]] to every HList produced by zipping the outputs of expressions that return the
    * same higher-kinded sequence, stopping at the shortest sequence.
    */
  // TODO: Rename to ZipOutputToShortest
  final case class ZipOutput[V, M[_] : Align : FunctorFilter, L <: HList, R, P](
    inputExprHList: NonEmptyExprHList[V, M, L, P],
    converter: ExprConverter[L, R],
    capture: CaptureP[V, M[R], P],
  ) extends Expr[V, M[R], P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[M[R]] = v.visitZipOutput(this)
  }

  // TODO: Figure out how to implement this so that it zips all existing elements of IEL into an HList of type DL
  //       (or fuses missing elements from empty M's with the appropriate value from the defaults HList of type DL)
  final case class ZipWithDefaults[V, M[_] : Align : Functor, DL <: HList, IEL <: ExprHList[V, P], P](
    inputExpr: IEL,
    defaultsExpr: Expr[V, DL, P],
    capture: CaptureP[V, M[DL], P],
  )(implicit
    val op: CanZipWithDefaults[V, P, M, DL, IEL],
  ) extends Expr[V, M[DL], P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[M[DL]] = v.visitZipWithDefaults(this)
  }

  /**
    * Return true if the output of the given [[inputExpr]] is an empty collection.
    */
  final case class OutputIsEmpty[V, M[_] : Foldable, R, P](
    inputExpr: Expr[V, M[R], P],
    capture: CaptureP[V, Boolean, P],
  ) extends Expr[V, Boolean, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[Boolean] = v.visitOutputIsEmpty(this)
  }

  /**
    * Take a given number of elements from the input sequence.
    *
    * If the number to take is positive, it will pull that number of elements from the start of the sequence.
    * If negative, it will pull that number of elements from the tail of the sequence. If zero, it will return
    * an empty sequence.
    */
  final case class TakeFromOutput[V, M[_] : Traverse : TraverseFilter, R, P](
    inputExpr: Expr[V, M[R], P],
    take: Int,
    capture: CaptureP[V, M[R], P],
  ) extends Expr[V, M[R], P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[M[R]] = v.visitTakeFromOutput(this)
  }

  /**
    * Returns `true` if at least one element in the output of the given [[inputExpr]] meets the given [[conditionExpr]].
    *
    * @note this <i>does not</i> short-circuit. [[Evidence]] for all elements that match (or do not match) the
    *       [[conditionExpr]] will be retained as evidence of the whole result.
    * @note if you want to apply this to the input (i.e. `F[V]`), you can pass [[ReturnInput]] as the [[inputExpr]].
    * @see [[Foldable.exists]] for more details.
    */
  final case class ExistsInOutput[V, M[_] : Foldable, U, P](
    inputExpr: Expr[V, M[U], P],
    conditionExpr: Expr[U, Boolean, P],
    capture: CaptureP[V, Boolean, P],
  ) extends Expr[V, Boolean, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[Boolean] = v.visitExistsInOutput(this)
  }

  /**
    * Adds the results of all expressions in [[inputExprList]] using the provided definition for [[Addition]].
    */
  final case class AddOutputs[V, R : Addition, P](
    inputExprList: NonEmptyList[Expr[V, R, P]],
    capture: CaptureP[V, R, P],
  ) extends Expr[V, R, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[R] = v.visitAddOutputs(this)
  }

  /**
    * Subtracts the results of all expressions in [[inputExprList]] using the provided definition for [[Subtraction]].
    *
    * @note the order of expressions matters for subtraction, so all subtraction is applied left-to-right.
    */
  final case class SubtractOutputs[V, R : Subtraction, P](
    inputExprList: NonEmptyList[Expr[V, R, P]],
    capture: CaptureP[V, R, P],
  ) extends Expr[V, R, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[R] = v.visitSubtractOutputs(this)
  }

  /**
    * Returns the negative result value of [[inputExpr]] using the provided definition for [[Negative]].
    */
  final case class NegativeOutput[V, R : Negative, P](
    inputExpr: Expr[V, R, P],
    capture: CaptureP[V, R, P],
  ) extends Expr[V, R, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[R] = v.visitNegativeOutput(this)
  }

  /**
    * Checks if the result of the [[inputExpr]] is contained within the given set of [[accepted]] values.
    */
  final case class OutputWithinSet[V, R, P](
    inputExpr: Expr[V, R, P],
    accepted: Set[R],
    capture: CaptureP[V, Boolean, P],
  ) extends Expr[V, Boolean, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[Boolean] = v.visitOutputWithinSet(this)
  }

  /**
    * Checks if the result of the [[inputExpr]] is contained within the given [[window]].
    *
    * This is effectively how comparison checks are made.
    *
    * @see [[Window]] for more details.
    */
  final case class OutputWithinWindow[V, R, P](
    inputExpr: Expr[V, R, P],
    window: Window[R],
    capture: CaptureP[V, Boolean, P],
  ) extends Expr[V, Boolean, P] {
    override def visit[G[_]](v: Visitor[V, P, G]): G[Boolean] = v.visitOutputWithinWindow(this)
  }
}

/**
  * A container for a condition and an expression to compute if the condition is met.
  *
  * @see [[Expr.When]] for usage.
  *
  * @param whenExpr a conditional expression that guards the resulting [[thenExpr]]
  * @param thenExpr an expression to compute the result if the [[whenExpr]] returns true
  */
final case class ConditionBranch[V, R, P](
  whenExpr: Expr[V, Boolean, P],
  thenExpr: Expr[V, R, P],
)
