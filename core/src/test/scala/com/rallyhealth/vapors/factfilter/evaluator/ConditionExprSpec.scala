package com.rallyhealth.vapors.factfilter.evaluator

import com.rallyhealth.vapors.factfilter.Example.{FactTypes, JoeSchmoe}
import com.rallyhealth.vapors.factfilter.data.{Evidence, FactSet}
import com.rallyhealth.vapors.factfilter.dsl.CaptureP.unit._
import org.scalatest.wordspec.AnyWordSpec
import com.rallyhealth.vapors.factfilter.dsl.ExprDsl._

class ConditionExprSpec extends AnyWordSpec {

  "Expr.OutputWithinSet" should {

    "find an asthma tag in a set that contains it" in {
      val q = withFactsOfType(FactTypes.Tag).where {
        _.exists {
          _.value.in(Set("asthma", "diabetes"))
        }
      }
      val result = eval(JoeSchmoe.factTable)(q)
      assert(result.output.value)
      assertResult(Evidence(JoeSchmoe.asthmaTag))(result.output.evidence)
    }

    "not find an asthma tag in a set that does not contain it" in {
      val q = withFactsOfType(FactTypes.Tag).where {
        _.exists {
          _.value.in(Set("diabetes"))
        }
      }
      val result = eval(JoeSchmoe.factTable)(q)
      assert(!result.output.value)
      // TODO: Should this contain the tags that did not match?
      assertResult(Evidence.none)(result.output.evidence)
    }
  }
}
