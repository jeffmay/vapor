package com.rallyhealth

package vapors.interpreter

import vapors.data.Evidence
import vapors.dsl._
import vapors.example.{FactTypes, JoeSchmoe}

import org.scalatest.wordspec.AnyWordSpec

class OutputWithinSetExprSpec extends AnyWordSpec {

  "Expr.OutputWithinSet" should {

    "find an asthma tag in a set that contains it" in {
      val q = valuesOfType(FactTypes.Tag).exists {
        _ in Set("asthma", "diabetes")
      }
      val result = eval(JoeSchmoe.factTable)(q)
      assert(result.output.value)
      assertResult(Evidence(JoeSchmoe.asthmaTag))(result.output.evidence)
    }

    "not find an asthma tag in a set that does not contain it" in {
      val q = valuesOfType(FactTypes.Tag).exists {
        _ in Set("diabetes")
      }
      val result = eval(JoeSchmoe.factTable)(q)
      assert(!result.output.value)
      assertResult(Evidence.none)(result.output.evidence)
    }
  }
}
