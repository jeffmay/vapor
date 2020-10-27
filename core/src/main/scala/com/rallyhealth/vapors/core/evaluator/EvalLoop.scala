package com.rallyhealth.vapors.core.evaluator

import cats.data.NonEmptyList
import cats.instances.function._
import cats.~>
import com.rallyhealth.vapors.core.algebra._
import com.rallyhealth.vapors.core.logic.{Intersect, Union}
import com.rallyhealth.vapors.factfilter.dsl.Exp

private[evaluator] final class EvalLoop[T] extends (ExpAlg[T, *] ~> (T => *)) {

  override def apply[A](fa: ExpAlg[T, A]): T => A = evalF(fa, _)

  private def evalLoop[U, B](exp: Exp[U, B]): U => B = {
    exp.foldMap(this.asInstanceOf[EvalLoop[U]])
  }

  private def evalF[B](
    exp: ExpAlg[T, B],
    data: T,
  ): B = exp match {
    case ExpAlg.Pure(_, value) =>
      value(data)
    case ExpAlg.Select(selector, expression) =>
      evalLoop(expression)(selector.get(data))
    case ExpAlg.Exists(toIterable, condition, foundTrue, whenFalse) =>
      val values = toIterable(data).iterator
      val results = values.map(v => (v, evalLoop(condition)(v)))
      val found = results.collect { case (v, true) => v }
      NonEmptyList.fromList(found.toList).map(foundTrue).getOrElse(whenFalse(data))
    case ExpAlg.ForAll(toIterable, condition, foundFalse, whenTrue) =>
      val values = toIterable(data).iterator
      val results = values.map(v => (v, evalLoop(condition)(v)))
      val found = results.collect { case (v, false) => v }
      NonEmptyList.fromList(found.toList).map(foundFalse).getOrElse(whenTrue(data))
    case exp @ ExpAlg.EqualTo(value, whenTrue, whenFalse) =>
      if (exp.eq.eqv(value, data)) whenTrue(data) else whenFalse(data)
    case ExpAlg.SetContains(set, whenTrue, whenFalse) =>
      if (set.contains(data)) whenTrue(data) else whenFalse(data)
    case ExpAlg.Within(window, whenTrue, whenFalse) =>
      if (window.contains(data)) whenTrue(data) else whenFalse(data)
    case ExpAlg.Cond(condition, thenExpression, elseExpression) =>
      val success = evalLoop(condition)(data)
      if (success) evalLoop(thenExpression)(data)
      else evalLoop(elseExpression)(data)
    case ExpAlg.Collect(_, collector, expression, whenEmpty) =>
      collector(data).map(evalLoop(expression)).getOrElse(whenEmpty(data))
    case ExpAlg.And(combine, expressions) =>
      combine(expressions.map(evalLoop[T, B]).map(_(data)))
    case ExpAlg.Or(combine, expressions) =>
      combine(expressions.map(evalLoop[T, B]).map(_(data)))
  }
}
