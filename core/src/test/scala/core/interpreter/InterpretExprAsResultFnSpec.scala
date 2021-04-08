package com.rallyhealth.vapors

package core.interpreter

import core.data.Evidence
import core.dsl._
import core.example.{FactTypes, JoeSchmoe}

import org.scalatest.wordspec.AnyWordSpec

class InterpretExprAsResultFnSpec extends AnyWordSpec {

  "InterpretExprAsFunction" when {

    "using no post processing" should {

      "find a single fact from a query" in {
        val q = factsOfType(FactTypes.Age).exists {
          _.get(_.select(_.value)) >= 18
        }
        val result = eval(JoeSchmoe.factTable)(q)
        assert(result.param.value === ())
        assert(result.output.value)
        assert(result.output.evidence.nonEmpty)
        assertResult(Evidence(JoeSchmoe.age))(result.output.evidence)
      }

      "find a complex fact from a query" in {
        val q = factsOfType(FactTypes.ProbabilityToUse).exists {
          _.getFoldable(_.select(_.value).select(_.scores).at("weightloss")).exists {
            _ > 0.5
          }
        }
        val result = eval(JoeSchmoe.factTable)(q)
        assertResult(Evidence(JoeSchmoe.probs))(result.output.evidence)
      }

      "define a fact expression" in {
        val likelyToJoinWeightloss = factsOfType(FactTypes.ProbabilityToUse).exists {
          _.getFoldable(_.select(_.value).select(_.scores).at("weightloss")).exists {
            _ > 0.5
          }
        }
        val result = eval(JoeSchmoe.factTable)(likelyToJoinWeightloss)
        assertResult(Evidence(JoeSchmoe.probs))(result.output.evidence)
      }
    }
  }
}
