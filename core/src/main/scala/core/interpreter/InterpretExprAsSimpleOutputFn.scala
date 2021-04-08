package com.rallyhealth.vapors

package core.interpreter

import core.algebra.Expr
import core.interpreter.InterpretExprAsSimpleOutputFn.GenSimpleOutput

import cats.{Eval, Functor, Semigroupal}

/**
  * Interprets an [[Expr]] as a function from [[ExprInput]] to a tuple of [[ExprOutput]] and a list of captured params.
  *
  * @see [[GenSimpleOutput]] for the interpreted function return type.
  *
  * This is necessary to define a [[Functor]] and [[Semigroupal]] for an expression, so that we can recursively map
  * over the whole [[com.rallyhealth.vapors.core.algebra.NonEmptyExprHList]] to get an [[shapeless.HList]] result.
  *
  * Since the [[InterpretExprAsResultFn]] return type is dependent on the input [[Expr]], it is impossible to define
  * these instances. By simplifying to the more generic return type of [[GenSimpleOutput]] we are able to leverage
  * the standard function, tuple, and list instances from cats over the well defined type constructor of [[ExprOutput]].
  */
class InterpretExprAsSimpleOutputFn[V, P] extends VisitGenericExprWithProxyFn[V, P, GenSimpleOutput[*, P]] {

  override protected def visitGeneric[U, R](
    expr: Expr[U, R, P],
    input: ExprInput[U],
  ): GenSimpleOutput[R, P] = {
    val result = InterpretExprAsResultFn(expr)(input)
    (result.output, result.param :: Nil)
  }
}

object InterpretExprAsSimpleOutputFn {
  type GenSimpleOutput[R, P] = (ExprOutput[R], List[Eval[P]])

  class SimpleOutputFnFunctorBuilder[V, P] {
    type SimpleOutput[R] = GenSimpleOutput[R, P]
    type SimpleOutputFn[R] = ExprInput[V] => SimpleOutput[R]

    implicit object SimpleOutputFunctor extends Functor[SimpleOutputFn] with Semigroupal[SimpleOutputFn] {
      override def map[A, B](fa: SimpleOutputFn[A])(f: A => B): SimpleOutputFn[B] = fa.andThen {
        case (o, params) => (o.copy(value = f(o.value)), params)
      }

      override def product[A, B](
        fa: SimpleOutputFn[A],
        fb: SimpleOutputFn[B],
      ): SimpleOutputFn[(A, B)] = { input =>
        val (a, aParams) = fa(input)
        val (b, bParams) = fb(input)
        (ExprOutput((a.value, b.value), a.evidence & b.evidence), aParams ::: bParams)
      }
    }
  }
}
