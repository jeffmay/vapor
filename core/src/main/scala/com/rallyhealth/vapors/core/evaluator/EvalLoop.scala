package com.rallyhealth.vapors.core.evaluator

import cats.instances.function._
import cats.~>
import com.rallyhealth.vapors.core.algebra._
import com.rallyhealth.vapors.core.dsl.AnyExp

private[evaluator] final class EvalLoop[T] extends (ExpAlg[T, *] ~> (T => *)) {

  override def apply[A](fa: ExpAlg[T, A]): T => A = evalF(fa, _)

  private def evalLoop[U, B](exp: AnyExp[U, B]): U => B = {
    exp.foldMap(this.asInstanceOf[EvalLoop[U]])
  }

  private def evalF[B](
    exp: ExpAlg[T, B],
    data: T,
  ): B = exp match {
    case ExpPure(_, value) =>
      value(data)
    case ExpSelectField(selector, expression) =>
      evalLoop(expression)(selector.get(data))
    case ExpExists(toIterable, condition, whenTrue, whenFalse) =>
      // we don't need this intermediate list and can remove it for performance,
      // but for the time being this is a good place to put a debugger
      val results = toIterable(data).iterator.map(evalLoop(condition)).toList
      val success = results.exists(identity[Boolean])
      if (success) whenTrue(data)
      else whenFalse(data)
    case ExpForAll(toIterable, condition, whenTrue, whenFalse) =>
      // we don't need this intermediate list and can remove it for performance,
      // but for the time being this is a good place to put a debugger
      val results = toIterable(data).iterator.map(evalLoop(condition)).toList
      val success = results.forall(identity[Boolean])
      if (success) whenTrue(data)
      else whenFalse(data)
    case ExpWithin(window, whenTrue, whenFalse) =>
      if (window.contains(data)) whenTrue(data) else whenFalse(data)
    case ExpCond(condition, thenExpression, elseExpression) =>
      val success = evalLoop(condition)(data)
      if (success) evalLoop(thenExpression)(data)
      else evalLoop(elseExpression)(data)
    case ExpCollect(_, collector, expression, whenEmpty) =>
      collector(data).map(evalLoop(expression)).getOrElse(whenEmpty(data))
    case ExpAnd(combine, expressions) =>
      combine(expressions.map(evalLoop[T, B]).map(_(data)))
    case ExpOr(combine, expressions) =>
      combine(expressions.map(evalLoop[T, B]).map(_(data)))
  }
}
