package com.rallyhealth.vapors

package core.time

import java.time.temporal.{Temporal, TemporalAmount}
import java.time._

/**
  * Defines the capability of adding or subtracting an amount of time to a temporal moment.
  */
trait ModifyTime[T, -D] {

  /**
    * Add a positive or negative temporal amount to a temporal moment.
    */
  def addTo(
    temporal: T,
    amount: D,
  ): T
}

object ModifyTime {

  def addTo[T, D](
    temporal: T,
    amount: D,
  )(implicit
    modifier: ModifyTime[T, D],
  ): T = modifier.addTo(temporal, amount)

  private final class OfTemporal[T <: Temporal, -D <: TemporalAmount] extends ModifyTime[T, D] {
    override def addTo(
      temporal: T,
      amount: D,
    ): T = amount.addTo(temporal).asInstanceOf[T]
  }

  @inline private def temporal[T <: Temporal, D <: TemporalAmount]: ModifyTime[T, D] = new OfTemporal[T, D]

  /**
    * Allows adding a [[Duration]] to a [[Instant]].
    *
    * An [[Instant]] does not support adding or subtracting year, month, or day [[Period]]s.
    */
  implicit val ofInstant: ModifyTime[Instant, Duration] = temporal

  /**
    * Allows adding a [[Period]] to a [[LocalDate]].
    *
    * A [[LocalDate]] does not support adding or subtracting nanosecond, minute, hour, etc [[Duration]]s.
    */
  implicit val ofLocalDate: ModifyTime[LocalDate, Period] = temporal

  /**
    * Allows adding any [[TemporalAmount]] to a [[LocalDateTime]].
    *
    * A [[LocalDateTime]] supports adding or subtracting time [[Duration]]s as well as date [[Period]]s.
    */
  implicit val ofLocalDateTime: ModifyTime[LocalDateTime, TemporalAmount] = temporal

  /**
    * Allows adding any [[TemporalAmount]] to a [[ZonedDateTime]].
    *
    * A [[ZonedDateTime]] supports adding or subtracting time [[Duration]]s as well as date [[Period]]s.
    */
  implicit val ofZonedDateTime: ModifyTime[ZonedDateTime, TemporalAmount] = temporal
}
