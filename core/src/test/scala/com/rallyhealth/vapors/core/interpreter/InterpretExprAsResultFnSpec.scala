package com.rallyhealth.vapors.core.interpreter

import com.rallyhealth.vapors.core.data.Evidence
import com.rallyhealth.vapors.core.dsl._
import com.rallyhealth.vapors.core.example.{FactTypes, JoeSchmoe}
import org.scalatest.wordspec.AnyWordSpec

class InterpretExprAsResultFnSpec extends AnyWordSpec {

  "InterpretExprAsFunction" when {

    "using no post processing" should {

      "find a single fact from a query" in {
        val q = valuesOfType(FactTypes.Age).exists {
          _ >= 18
        }
        val result = eval(JoeSchmoe.factTable)(q)
        assert(result.param.value === ())
        assert(result.output.value)
        assert(result.output.evidence.nonEmpty)
        assertResult(Evidence(JoeSchmoe.age))(result.output.evidence)
      }

      "find a complex fact from a query" in {
        val q = valuesOfType(FactTypes.ProbabilityToUse).exists {
          _.getFoldable(_.select(_.scores).at("weightloss")).exists {
            _ > 0.5
          }
        }
        val result = eval(JoeSchmoe.factTable)(q)
        assertResult(Evidence(JoeSchmoe.probs))(result.output.evidence)
      }

      "define a fact expression" in {
        val likelyToJoinWeightloss = valuesOfType(FactTypes.ProbabilityToUse).exists {
          _.getFoldable(_.select(_.scores).at("weightloss")).exists {
            _ > 0.5
          }
        }
        val result = eval(JoeSchmoe.factTable)(likelyToJoinWeightloss)
        assertResult(Evidence(JoeSchmoe.probs))(result.output.evidence)
      }
    }
  }
}
