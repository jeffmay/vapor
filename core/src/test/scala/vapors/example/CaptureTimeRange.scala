package com.rallyhealth

package vapors.example

import vapors.algebra.{CaptureP, Expr}
import vapors.data.{ExtractInstant, TypedFact}
import vapors.interpreter.{ExprInput, ExprOutput}

import cats.{Eval, Monoid}

import java.time.Instant

object CaptureTimeRange extends CaptureP.AsMonoidCompanion[TimeRange] {

  implicit def captureTimeRangeFromFacts[T : ExtractInstant, R]: CaptureP.AsMonoidFromFactsOfType[T, R, TimeRange] = {
    new CaptureP.AsMonoidFromFactsOfType[T, R, TimeRange] {

      override protected def foldWithParentParam(
        expr: Expr[Seq[TypedFact[T]], R, TimeRange],
        input: ExprInput[Seq[TypedFact[T]]],
        output: ExprOutput[R],
        processedChildren: TimeRange,
      ): Eval[TimeRange] = {
        val timestamps = input.value.map(fact => ExtractInstant[T].extractValue(fact.value))
        Eval.now(TimeRange.fromIterable(timestamps))
      }
    }
  }
}

/**
  * A range of time that tracks the start and end of the range, without any concern for the values inbetween.
  *
  * @param min the earliest timestamp in the evaluated expression
  * @param max the oldest timestamp in the evaluated expression
  */
final case class TimeRange private (
  min: Option[Instant],
  max: Option[Instant],
) {

  /**
    * Build a new [[TimeRange]] from the earliest start and latest end time of both the given range and this.
    */
  def expand(that: TimeRange): TimeRange =
    if (this eq TimeRange.empty) that
    else if (that eq TimeRange.empty) this
    else TimeRange.fromIterable(Array(this.min, that.min, this.max, that.max).flatten[Instant])
}

object TimeRange {

  final val empty = TimeRange(None, None)

  @inline final def apply(): TimeRange = empty

  def apply(one: Instant): TimeRange = TimeRange(Some(one), Some(one))

  def apply(
    min: Instant,
    max: Instant,
  ): TimeRange = TimeRange(Some(min), Some(max))

  def fromIterable(many: Iterable[Instant]): TimeRange = {
    if (many.isEmpty) empty
    else {
      val sorted = Array.from(many).sortInPlace()
      TimeRange(sorted.headOption, sorted.lastOption)
    }
  }

  implicit val monoid: Monoid[TimeRange] = {
    new Monoid[TimeRange] {
      override def empty: TimeRange = TimeRange.empty
      override def combine(
        x: TimeRange,
        y: TimeRange,
      ): TimeRange = x.expand(y)
    }
  }
}
