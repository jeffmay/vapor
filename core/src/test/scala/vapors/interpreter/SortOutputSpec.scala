package com.rallyhealth

package vapors.interpreter

import vapors.data.FactTable
import vapors.dsl._
import vapors.example.{BloodPressure, FactTypes}

import org.scalatest.freespec.AnyFreeSpec

import java.time.Instant

class SortOutputSpec extends AnyFreeSpec {

  "Expr.SortOutput" - {

    val bpNow = Instant.now()
    val bp5MinAgo = bpNow.minusSeconds(5 * 60)
    val bp15MinAgo = bpNow.minusSeconds(15 * 60)
    val lowDiastolic = BloodPressure(70, 100, bp15MinAgo)
    val medDiastolic = BloodPressure(80, 100, bp5MinAgo)
    val highDiastolic = BloodPressure(90, 100, bpNow)

    val bpFacts = FactTable(
      Seq(
        lowDiastolic,
        medDiastolic,
        highDiastolic,
      ).map(FactTypes.BloodPressureMeasurement(_)),
    )

    "sorted using natural ordering" in {
      val query = factsOfType(FactTypes.BloodPressureMeasurement).map(_.value.get(_.select(_.diastolic))).sorted
      assertResult(Some(highDiastolic)) {
        bpFacts.getSortedSeq(FactTypes.BloodPressureMeasurement).headOption.map(_.value)
      }
      val result = eval(bpFacts)(query)
      assertResult(lowDiastolic.diastolic) {
        result.output.value.head
      }
    }

    "sortBy ordered field" in {
      val query = factsOfType(FactTypes.BloodPressureMeasurement).map(_.value).sortBy(_.select(_.diastolic))
      assertResult(Some(highDiastolic)) {
        bpFacts.getSortedSeq(FactTypes.BloodPressureMeasurement).headOption.map(_.value)
      }
      val result = eval(bpFacts)(query)
      assertResult(lowDiastolic) {
        result.output.value.head
      }
    }
  }
}
