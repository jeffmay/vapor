package com.rallyhealth

package vapors.data

import vapors.example.{FactTypes, GenericMeasurement}

import org.scalatest.wordspec.AnyWordSpec

import java.time.Instant

class FactTableSpec extends AnyWordSpec {

  "FactTable" when {

    "given facts that have the same value sort order" should {
      val now = Instant.now()
      val measurementsAtSameTime = Seq(
        GenericMeasurement("systolic", 120, "mmHg", now),
        GenericMeasurement("diastolic", 80, "mmHg", now),
      ).map(FactTypes.GenericMeasurement(_))
      val unsortedMeasurements = measurementsAtSameTime.toSet
      // GenericMeasurements are only sorted by timestamp, so these should all produce a comparison of 0
      val sortedMeasurements =
        measurementsAtSameTime.sorted(TypedFact.orderTypedFactByValue[GenericMeasurement].toOrdering)

      "not use that Order instance to remove duplicates" in {
        assertResult(unsortedMeasurements) {
          FactTable(measurementsAtSameTime).getSet(FactTypes.GenericMeasurement)
        }
      }

      "return the results as a Seq in the expected sort order" in {
        assertResult(sortedMeasurements) {
          FactTable(measurementsAtSameTime).getSortedSeq(FactTypes.GenericMeasurement)
        }
      }
    }
  }
}
