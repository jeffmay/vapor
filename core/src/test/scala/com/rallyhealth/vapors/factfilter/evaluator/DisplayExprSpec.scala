package com.rallyhealth.vapors.factfilter.evaluator

import com.rallyhealth.vapors.factfilter.Example.FactTypes
import com.rallyhealth.vapors.factfilter.dsl.CaptureP.unit._
import org.scalatest.wordspec.AnyWordSpec
import com.rallyhealth.vapors.factfilter.dsl.ExprDsl._

class DisplayExprSpec extends AnyWordSpec {

  "DisplayExpr" should {

    "display returnInput" in {
      val expr = withFactsOfType(FactTypes.Age).returnInput
      assertResult {
        """withFactsOfType(FactType('age' as Int)).where(_.returnInput)"""
      } {
        DisplayExpr.serialize(expr)
      }
    }
  }
}
