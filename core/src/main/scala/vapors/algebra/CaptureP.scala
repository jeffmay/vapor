package com.rallyhealth

package vapors.algebra

import vapors.data.TypedFact
import vapors.interpreter.{ExprInput, ExprOutput}

import cats.{Eval, Monoid}

import scala.annotation.implicitNotFound

@implicitNotFound(
  """
Missing implicit CaptureP[${V}, ${R}, P] (where P is a custom parameter type that you choose via implicit).
Currently, P is inferred as ${P}.

Expression builders rely on the implicit CaptureP in scope to determine the concrete type of P.
CaptureP.unit will be used by default unless another implicit is in scope and P will be inferred as Unit.
However, if the compiler has trouble inferring other arguments, such as V or R, then it may also fail to infer P.

Is the compiler is unable to infer either V (${V}) or R (${R})?
If so, you may need to check the types of your expression and make sure they match.

Failed to find a CaptureP for the following expression:
""",
)
trait CaptureP[V, R, P] {

  /**
    * Folds the captured parameters of the sub-expressions with the captured parameter of this expression.
    *
    * @param expr the expression that is being evaluated
    * @param input the input to the expression
    * @param output the output of the expression given the input
    * @param subExprParams the captured parameters of the sub-expressions.
    *                      NOTE: This is not a good data structure for parameters because you cannot tell where they
    *                      come from.
    */
  def foldToParam(
    expr: Expr[V, R, P],
    input: ExprInput[V],
    output: ExprOutput[R],
    subExprParams: List[Eval[P]],
  ): Eval[P]
}

object CaptureP extends CaptureUnitLowPriorityImplicit {

  /**
    * Captures the type parameter from facts with a value of type [[T]].
    */
  trait FromFactsOfType[T, R, P] extends CaptureP[Seq[TypedFact[T]], R, P]

  /**
    * Captures the type parameter from facts with a value of type [[T]] (assuming [[P]] is a [[Monoid]])
    *
    * @see [[AsMonoid]]
    */
  abstract class AsMonoidFromFactsOfType[T, R, P : Monoid]
    extends AsMonoid[Seq[TypedFact[T]], R, P] // TODO: Use Iterable instead of Seq?
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
  abstract class AsMonoid[V, R, P : Monoid] extends CaptureP[V, R, P] {

    protected val empty: P = Monoid[P].empty

    override def foldToParam(
      expr: Expr[V, R, P],
      input: ExprInput[V],
      output: ExprOutput[R],
      subExprParams: List[Eval[P]],
    ): Eval[P] = {
      val combinedChildren = Monoid[Eval[P]].combineAll(subExprParams)
      combinedChildren.flatMap(foldWithParentParam(expr, input, output, _))
    }

    protected def foldWithParentParam(
      expr: Expr[V, R, P],
      input: ExprInput[V],
      output: ExprOutput[R],
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
  final class AsMonoidAndPass[V, R, P : Monoid] extends AsMonoid[V, R, P] {

    override protected def foldWithParentParam(
      expr: Expr[V, R, P],
      input: ExprInput[V],
      output: ExprOutput[R],
      processedChildren: P,
    ): Eval[P] = Eval.now(processedChildren)

    override def toString: String = "captureParamAndPass"
  }

  /**
    * Extend this in your companion object to get a low-priority implicit for capturing your [[Monoid]] parameter
    * for types that you don't specifically select.
    */
  abstract class AsMonoidCompanion[P](protected implicit final val P: Monoid[P]) {
    implicit def captureParamAndPass[V, R]: CaptureP[V, R, P] = new AsMonoidAndPass[V, R, P]
  }
}

sealed trait CaptureUnitLowPriorityImplicit {

  implicit def captureUnit[V, R]: CaptureP[V, R, Unit] = new CaptureP[V, R, Unit] {

    override final def foldToParam(
      expr: Expr[V, R, Unit],
      input: ExprInput[V],
      output: ExprOutput[R],
      subExprParams: List[Eval[Unit]],
    ): Eval[Unit] = Eval.Unit

    override final def toString: String = "captureUnit"
  }
}
