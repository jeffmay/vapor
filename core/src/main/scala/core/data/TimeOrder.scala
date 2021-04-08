package com.rallyhealth.vapors

package core.data

import cats.syntax.contravariant._
import cats.Order

import java.time._
import java.time.chrono.{ChronoLocalDate, ChronoLocalDateTime, ChronoZonedDateTime}

abstract class TimeOrder {

  def modify[T](order: Order[T]): Order[T]

  implicit val latestLocalDateFirst: Order[LocalDate] =
    Order.reverse(Order.fromComparable[ChronoLocalDate].contramap(identity[ChronoLocalDate]))

  implicit val latestLocalDateTimeFirst: Order[LocalDateTime] =
    Order.reverse(
      Order
        .fromComparable[ChronoLocalDateTime[_ <: ChronoLocalDate]]
        .contramap(identity[ChronoLocalDateTime[_ <: ChronoLocalDate]]),
    )

  implicit val latestLocalTimeFirst: Order[LocalTime] = Order.reverse(Order.fromComparable)

  implicit val latestInstantFirst: Order[Instant] = Order.reverse(Order.fromComparable)

  implicit val latestZonedDateTimeFirst: Order[ZonedDateTime] =
    Order.reverse(
      Order
        .fromComparable[ChronoZonedDateTime[_ <: ChronoLocalDate]]
        .contramap(identity[ChronoZonedDateTime[_ <: ChronoLocalDate]]),
    )
}

object TimeOrder {

  final object LatestFirst extends TimeOrder {
    override def modify[T](order: Order[T]): Order[T] = Order.reverse(order)
  }

  final object EarliestFirst extends TimeOrder {
    override def modify[T](order: Order[T]): Order[T] = order
  }
}
