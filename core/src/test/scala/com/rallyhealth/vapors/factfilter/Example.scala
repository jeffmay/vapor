package com.rallyhealth.vapors.factfilter

import java.time.{Instant, LocalDate, ZoneOffset}

import cats.{Eq, Order}
import cats.data.NonEmptyList
import com.rallyhealth.vapors.factfilter.data.{FactTable, FactType, FactTypeSet}
import com.rallyhealth.vapors.factfilter.extras.ExtractInstant

object Example {

  final case class Probs(scores: Map[String, Double])

  sealed trait Role
  final object Role {
    case object Admin extends Role
    case object User extends Role

    implicit val eq: Eq[Role] = Eq.fromUniversalEquals
    implicit val ordering: Ordering[Role] = {
      Ordering
        .by[Role, Int] {
          case Admin => 1
          case User => 2
        }
        .reverse
    }
    implicit val order: Order[Role] = Order.fromOrdering
  }

  trait HasTimestamp {
    def timestamp: Instant
  }

  implicit val extractTimestamp: ExtractInstant[HasTimestamp] = _.timestamp

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

  final object FactTypes {
    val Name = FactType[String]("name")
    val Age = FactType[Int]("age")
    val Role = FactType[Role]("role")
    val BirthYear = FactType[Int]("year_of_birth")
    val DateOfBirth = FactType[LocalDate]("date_of_birth")
    val WeightMeasurement = FactType[WeightMeasurementLbs]("weight_measurement")
    val WeightSelfReported = FactType[WeightMeasurementLbs]("weight_self_reported")
    val BloodPressureMeasurement = FactType[BloodPressure]("blood_pressure")
    val Tag = FactType[String]("tag")
    val ProbabilityToUse = FactType[Probs]("probability_to_use")
  }

  final object FactTypeSets {
    import FactTypes._
    val Weight = FactTypeSet.of(WeightMeasurement, WeightSelfReported)
  }

  final object JoeSchmoe {
    val name = FactTypes.Name("Joe Schmoe")
    val age = FactTypes.Age(32)
    val userRole = FactTypes.Role(Role.User)
    val adminRole = FactTypes.Role(Role.Admin)
    val dateOfBirth = FactTypes.DateOfBirth(LocalDate.of(1987, 1, 1))

    val weight = FactTypes.WeightMeasurement(
      WeightMeasurementLbs(250.0, LocalDate.of(2020, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC)),
    )

    val weightSelfReported = FactTypes.WeightSelfReported(
      WeightMeasurementLbs(240.0, LocalDate.of(2019, 1, 30).atStartOfDay().toInstant(ZoneOffset.UTC)),
    )

    val bloodPressure = FactTypes.BloodPressureMeasurement(
      BloodPressure(120, 80, LocalDate.of(2020, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC)),
    )

    val probs = FactTypes.ProbabilityToUse(Probs(Map("weightloss" -> .8)))
    val asthmaTag = FactTypes.Tag("asthma")

    val facts = NonEmptyList.of(
      name,
      age,
      adminRole,
      userRole,
      weight,
      weightSelfReported,
      bloodPressure,
      probs,
      asthmaTag,
    )

    val factTable = FactTable(facts.toList)
  }
}
