package com.rallyhealth.vapors.factfilter.extras

import cats.Eval
import com.rallyhealth.vapors.core.algebra.Expr
import com.rallyhealth.vapors.factfilter.data.TypedFact
import com.rallyhealth.vapors.factfilter.dsl.CaptureP
import com.rallyhealth.vapors.factfilter.evaluator.InterpretExprAsFunction

import scala.collection.immutable.SortedSet

object CaptureTimeRange extends CaptureP.AsMonoidCompanion[TimeRange] {

  implicit def captureTimeRangeFromFacts[T : ExtractInstant, R]: CaptureP.AsMonoidFromFactsOfType[T, R, TimeRange] = {
    new CaptureP.AsMonoidFromFactsOfType[T, R, TimeRange] {

      override protected def foldWithParentParam(
        expr: Expr[SortedSet, TypedFact[T], R, TimeRange],
        input: InterpretExprAsFunction.Input[SortedSet, TypedFact[T]],
        output: InterpretExprAsFunction.Output[R],
        processedChildren: TimeRange,
      ): Eval[TimeRange] = {
        val timestamps = input.value.map(fact => ExtractInstant[T].extractValue(fact.value))
        Eval.now(TimeRange.fromIterable(timestamps))
      }
    }
  }
}
