package com.rallyhealth

package vapors.algebra

import vapors.data.Evidence
import vapors.dsl._
import vapors.example.{FactTypes, JoeSchmoe, TimeRange}

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.wordspec.AnyWordSpec

class CapturePSpec extends AnyWordSpec with TypeCheckedTripleEquals {

  "CaptureP" should {

    "capture a timestamp range post processing param" should {

      import vapors.example.CaptureTimeRange._

      "find a single fact from a query" in {
        val q = factsOfType(FactTypes.WeightMeasurement).exists {
          _.get(_.select(_.value).select(_.value)) > 18
        }
        val result = eval(JoeSchmoe.factTable)(q)
        assert(result.param.value === TimeRange(JoeSchmoe.weight.value.timestamp))
        assert(result.output.value)
        assert(result.output.evidence.nonEmpty)
        assertResult(Evidence(JoeSchmoe.weight))(result.output.evidence)
      }
    }
  }
}
