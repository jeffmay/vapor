package com.rallyhealth.vapors.factfilter.extras

import cats.Eval
import com.rallyhealth.vapors.core.algebra.Expr
import com.rallyhealth.vapors.factfilter.data.TypedFact
import com.rallyhealth.vapors.factfilter.dsl.CaptureP
import com.rallyhealth.vapors.factfilter.evaluator.InterpretExprAsFunction

object CaptureTimeRange extends CaptureP.AsMonoidCompanion[TimeRange] {

  implicit def captureTimeRangeFromFacts[T : ExtractInstant, R]: CaptureP[List, TypedFact[T], R, TimeRange] = {
    new CaptureP.AsMonoid[List, TypedFact[T], R, TimeRange] {

      override protected def foldWithParentParam(
        expr: Expr[List, TypedFact[T], R, TimeRange],
        input: InterpretExprAsFunction.Input[List, TypedFact[T]],
        output: InterpretExprAsFunction.Output[R],
        processedChildren: TimeRange,
      ): Eval[TimeRange] = {
        val timestamps = input.value.map(fact => ExtractInstant[T].extractValue(fact.value))
        Eval.now(TimeRange.fromIterable(timestamps))
      }
    }
  }
}
