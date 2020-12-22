package com.rallyhealth.vapors.factfilter.evaluator

import com.rallyhealth.vapors.factfilter.Example._
import com.rallyhealth.vapors.factfilter.data.Evidence
import com.rallyhealth.vapors.factfilter.dsl.ExprDsl._
import org.scalatest.wordspec.AnyWordSpec

class InterpretExprAsFunctionSpec extends AnyWordSpec {

  import cats.instances.all._

  "InterpretExprAsFunction" when {

    "using no post processing" should {

      "find a single fact from a query" in {
        val q = withFactsOfType(FactTypes.Age).where {
          _.exists {
            _.get(_.select(_.value)) >= 18
          }
        }
        val result = eval(JoeSchmoe.factTable)(q)
        assert(result.param.value === ())
        assert(result.output.value)
        assert(result.output.evidence.nonEmpty)
        assertResult(Evidence(JoeSchmoe.age))(result.output.evidence)
      }

      "find a complex fact from a query" in {
        val q = withFactsOfType(FactTypes.ProbabilityToUse).where {
          _.exists {
            _.getFoldable(_.select(_.value).select(_.scores).atKey("weightloss")).exists {
              _ > 0.5
            }
          }
        }
        val result = eval(JoeSchmoe.factTable)(q)
        assertResult(Evidence(JoeSchmoe.probs))(result.output.evidence)
      }

      "define a fact expression" in {
        val likelyToJoinWeightloss = withFactsOfType(FactTypes.ProbabilityToUse).where {
          _.exists {
            _.getFoldable(_.select(_.value).select(_.scores).atKey("weightloss")).exists {
              _ > 0.5
            }
          }
        }
        val result = eval(JoeSchmoe.factTable)(likelyToJoinWeightloss)
        assertResult(Evidence(JoeSchmoe.probs))(result.output.evidence)
      }
    }
  }
}
