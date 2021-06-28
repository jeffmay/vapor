package com.rallyhealth

package vapors.data

import java.time._

/**
  * Special case of [[ExtractValue]] that extracts an [[Instant]].
  *
  * TODO: This should be replaced by a type alias to [[ExtractValue]] in the same way [[ExtractBoolean]] was.
  */
trait ExtractInstant[-T] extends ExtractValue[T, Instant]

object ExtractInstant {

  def apply[T](implicit instance: ExtractInstant[T]): ExtractInstant[T] = instance

  def from[T, V : ExtractInstant](extractField: T => V): ExtractInstant[T] = { obj =>
    ExtractInstant[V].extractValue(extractField(obj))
  }

  implicit def instantFromLocalDate(implicit zone: ZoneId): ExtractInstant[LocalDate] = _.atStartOfDay(zone).toInstant

  implicit def instantFromLocalDateTime(implicit zone: ZoneId): ExtractInstant[LocalDateTime] = _.atZone(zone).toInstant

  implicit val instantFromZonedDate: ExtractInstant[ZonedDateTime] = _.toInstant

  implicit val instantFromZonedDateTime: ExtractInstant[ZonedDateTime] = _.toInstant
}
