package com.rallyhealth

package vapors.example

import vapors.data.TimeOrder.LatestFirst._
import vapors.data._

import cats.Order

import java.time.Instant

final case class Probs(scores: Map[String, Double])

object Probs {
  implicit val order: Order[Probs] = Order.whenEqual(
    Order.reverse(Order.by(_.scores.size)),
    Order.by(_.scores.toSeq),
  )
}

sealed trait Role

object Role {
  case object Admin extends Role
  case object User extends Role

  implicit val order: Order[Role] = Order.reverse {
    Order.by[Role, Int] {
      case Admin => 1
      case User => 2
    }
  }
}

sealed trait ColorCoding

object ColorCoding {
  final case object Red extends ColorCoding
  final case object Green extends ColorCoding
  final case object Blue extends ColorCoding
}

trait HasTimestamp {
  def timestamp: Instant
}

object HasTimestamp {
  implicit val extractTimestamp: ExtractInstant[HasTimestamp] = _.timestamp
  implicit def orderByTimestampLatestFirst[T <: HasTimestamp]: Order[T] = Order.by(_.timestamp)
}

sealed trait Measurement extends HasTimestamp {
  def name: String
}

sealed trait NumericMeasurement extends Measurement {
  def value: Double
  def unit: String
}

final case class BloodPressure(
  diastolic: Double,
  systolic: Double,
  timestamp: Instant,
) extends Measurement {
  override def name: String = "blood_pressure"
}

final case class WeightMeasurementLbs(
  value: Double,
  timestamp: Instant,
) extends NumericMeasurement {
  override def name: String = "weight"
  override def unit: String = "lbs"
}

final case class GenericMeasurement(
  name: String,
  value: Double,
  unit: String,
  timestamp: Instant,
) extends Measurement

final case class TagsUpdate(
  source: String,
  tags: Set[String],
  timestamp: Instant,
) extends HasTimestamp

final case class Address(
  street1: String,
  street2: String,
  city: String,
  state: String,
  zip: String,
)

final case class AddressUpdate(
  address: Address,
  timestamp: Instant,
) extends HasTimestamp
