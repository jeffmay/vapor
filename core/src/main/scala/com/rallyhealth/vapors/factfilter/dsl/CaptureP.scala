package com.rallyhealth.vapors.factfilter.dsl

import cats.{Eval, Monoid}
import com.rallyhealth.vapors.core.algebra.Expr
import com.rallyhealth.vapors.factfilter.data.TypedFact
import com.rallyhealth.vapors.factfilter.evaluator.InterpretExprAsFunction.{Input, Output}

import scala.annotation.implicitNotFound
import scala.collection.immutable.SortedSet

@implicitNotFound(
  """
Missing implicit CaptureP[${F}, ${V}, ${R}, P] (where P is some arbitrary type of parameter).

If you do not plan to do post-processing, you can import CaptureP.unit._

Alternatively, you can import or define a CaptureP that matches the type of this expression node and extracts some type parameter P.
If you decide to define it, remember to define one implicit where all of the types, aside from P, are abstract use a low priority implicit trait.
It will need to combine, return, or ignore the parameters from its child nodes. If P is a Monoid, you can just define it using CaptureP.AsMonoid.

The expression node that failed to find a post processor is:
""",
)
trait CaptureP[F[_], V, R, P] {

  def foldToParam(
    expr: Expr[F, V, R, P],
    input: Input[F, V],
    output: Output[R],
    subExprParams: List[Eval[P]],
  ): Eval[P]
}

object CaptureP {

  /**
    * Captures the type parameter from facts with a value of type [[T]].
    */
  trait FromFactsOfType[T, R, P] extends CaptureP[SortedSet, TypedFact[T], R, P]

  /**
    * Captures the type parameter from facts with a value of type [[T]] (assuming [[P]] is a [[Monoid]])
    *
    * @see [[AsMonoid]]
    */
  abstract class AsMonoidFromFactsOfType[T, R, P : Monoid]
    extends AsMonoid[SortedSet, TypedFact[T], R, P]
    with FromFactsOfType[T, R, P]

  // TODO: This is not very safe. Nodes will often combine the params of their input expressions and sub expressions.
  //       It might not be safe to assume that Monoid is enough to capture the expected behavior for combining the
  //       parameters in this situation. Maybe there is a way to group sub-expressions better than List?
  /**
    * Capture a [[Monoid]]-like parameter and combine all captured params of the current node's children.
    *
    * You must still define how to extract the parameter from the current node and combine it with its children.
    *
    * @note some [[Expr]] nodes lump params from different sub-expressions into the same list. If this doesn't
    *       work for you, then you must define a [[CaptureP]] and handle combining these sub-expression results
    *       in whatever way works for you.
    *
    * @note this can be used in conjunction with extending [[AsMonoidCompanion]] in the surrounding object
    *       to define a standard fallback for unmatched types (assuming your parameter is a [[Monoid]]).
    */
  abstract class AsMonoid[F[_], V, R, P : Monoid] extends CaptureP[F, V, R, P] {

    protected val empty: P = Monoid[P].empty

    override def foldToParam(
      expr: Expr[F, V, R, P],
      input: Input[F, V],
      output: Output[R],
      subExprParams: List[Eval[P]],
    ): Eval[P] = {
      val combinedChildren = Monoid[Eval[P]].combineAll(subExprParams)
      combinedChildren.flatMap(foldWithParentParam(expr, input, output, _))
    }

    protected def foldWithParentParam(
      expr: Expr[F, V, R, P],
      input: Input[F, V],
      output: Output[R],
      processedChildren: P,
    ): Eval[P]
  }

  /**
    * Captures all child node params, combines them, and passes them up as this node's param.
    *
    * @note this ignores the context of the node it matches. This should only be used as a fallback
    *       for having no definition of how to capture a parameter with the given types, but needing
    *       to pass the captured params from this node's children up to the next parent.
    */
  final class AsMonoidAndPass[F[_], V, R, P : Monoid] extends AsMonoid[F, V, R, P] {
    override protected def foldWithParentParam(
      expr: Expr[F, V, R, P],
      input: Input[F, V],
      output: Output[R],
      processedChildren: P,
    ): Eval[P] = Eval.now(processedChildren)
  }

  /**
    * Extend this in your companion object to get a low-priority implicit for capturing your [[Monoid]] parameter
    * for types that you don't specifically select.
    */
  abstract class AsMonoidCompanion[P](protected implicit final val P: Monoid[P]) {
    implicit def captureParamAndPass[F[_], V, R]: CaptureP[F, V, R, P] = new AsMonoidAndPass[F, V, R, P]
  }

  object unit {

    implicit def captureUnit[F[_], V, R]: CaptureP[F, V, R, Unit] = (_, _, _, _) => Eval.Unit
  }
}
