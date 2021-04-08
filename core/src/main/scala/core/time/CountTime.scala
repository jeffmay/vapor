package com.rallyhealth.vapors

package core.time

import java.time.temporal.{ChronoUnit, Temporal, TemporalUnit}

trait CountTime[-T, -U] {

  /**
    * Count the whole number of complete time units between the start and end.
    *
    * @param start the start of the time range (inclusive)
    * @param end the end of the time range (inclusive)
    * @param truncateToUnit the unit to round down to
    * @return the whole number of units between the start up to, but not including, the end
    */
  def between(
    start: T,
    end: T,
    truncateToUnit: U,
  ): Long
}

object CountTime {

  def between[T, U](
    start: T,
    end: T,
    roundDownToUnit: U,
  )(implicit
    diffTime: CountTime[T, U],
  ): Long = diffTime.between(start, end, roundDownToUnit)

  private final class OfTemporal[-T <: Temporal] extends CountTime[T, TemporalUnit] {
    override def between(
      start: T,
      end: T,
      unit: TemporalUnit,
    ): Long = start.until(end, unit)
  }

  @inline private def temporal[T <: Temporal]: CountTime[T, ChronoUnit] = new OfTemporal[T]

  // TODO: Restrict invalid units by type
  implicit val ofTemporal: CountTime[Temporal, ChronoUnit] = temporal
}
