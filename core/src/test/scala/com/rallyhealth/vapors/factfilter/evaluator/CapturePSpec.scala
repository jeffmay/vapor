package com.rallyhealth.vapors.factfilter.evaluator

import cats.instances.double._
import com.rallyhealth.vapors.factfilter.Example.{FactTypes, JoeSchmoe}
import com.rallyhealth.vapors.factfilter.data.Evidence
import com.rallyhealth.vapors.factfilter.dsl.ExprDsl._
import com.rallyhealth.vapors.factfilter.extras.TimeRange
import org.scalatest.wordspec.AnyWordSpec

class CapturePSpec extends AnyWordSpec {

  "CaptureP" should {

    "capture a timestamp range post processing param" should {

      import com.rallyhealth.vapors.factfilter.extras.CaptureTimeRange._

      "find a single fact from a query" in {
        val q = withFactsOfType(FactTypes.WeightMeasurement).where {
          _.exists {
            _.get(_.select(_.value).select(_.value)) > 18
          }
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
