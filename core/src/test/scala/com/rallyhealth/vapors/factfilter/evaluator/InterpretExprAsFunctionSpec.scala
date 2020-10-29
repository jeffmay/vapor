package com.rallyhealth.vapors.factfilter.evaluator

import com.rallyhealth.vapors.factfilter.Example._
import com.rallyhealth.vapors.factfilter.data.FactsMatch
import com.rallyhealth.vapors.factfilter.dsl.ExprDsl._
import com.rallyhealth.vapors.factfilter.dsl.Facts
import org.scalatest.wordspec.AnyWordSpec

class InterpretExprAsFunctionSpec extends AnyWordSpec {

  import cats.instances.all._

  "InterpretExprAsFunction" when {

    "using no post processing" should {

      import com.rallyhealth.vapors.factfilter.dsl.CaptureP.unit._

      "find a single fact from a query" in {
        val q = withFactsOfType(FactTypes.Age).where {
          _.exists {
            _.get(_.select(_.value)) >= 18
          }
        }
        val output = eval(JoeSchmoe.factTable)(q)
        assert(output.param.value === ())
        assert(output.output.value)
        assert(output.output.evidence.nonEmpty)
        assertResult(FactsMatch(Facts(JoeSchmoe.age)))(output.output.evidence)
      }

      "find a complex fact from a query" in {
        val q = withFactsOfType(FactTypes.ProbabilityToUse).where {
          _.exists {
            _.getFoldable(_.select(_.value).select(_.scores).atKey("weightloss")).exists {
              _ > 0.5
            }
          }
        }
        val output = eval(JoeSchmoe.factTable)(q)
        assertResult(FactsMatch(Facts(JoeSchmoe.probs)))(output.output.evidence)
      }

      "define a fact expression" in {
        val likelyToJoinWeightloss = withFactsOfType(FactTypes.ProbabilityToUse).where {
          _.exists {
            _.getFoldable(_.select(_.value).select(_.scores).atKey("weightloss")).exists {
              _ > 0.5
            }
          }
        }
        val output = eval(JoeSchmoe.factTable)(likelyToJoinWeightloss)
        assertResult(FactsMatch(Facts(JoeSchmoe.probs)))(output.output.evidence)
      }
    }
  }
}
