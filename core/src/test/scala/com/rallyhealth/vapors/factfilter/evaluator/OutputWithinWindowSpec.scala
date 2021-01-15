package com.rallyhealth.vapors.factfilter.evaluator

import com.rallyhealth.vapors.factfilter.data.FactTable
import org.scalatest.wordspec.AnyWordSpec
import com.rallyhealth.vapors.factfilter.dsl.ExprDsl._

class OutputWithinWindowSpec extends AnyWordSpec {

  "Expr.OutputWithinWindow" when {

    "using the === operator" should {

      "return 'true' when the values are equal" in {
        val q = const(2 + 2) === 4
        val result = eval(FactTable.empty)(q)
        assert(result.output.value)
      }

      "return 'false' when the values are not equal" in {
        val q = const(2 + 2) === 5
        val result = eval(FactTable.empty)(q)
        assert(!result.output.value)
      }
    }

    "using the !== operator" should {

      "return 'false' when the values are equal" in {
        val q = const(2 + 2) !== 4
        val result = eval(FactTable.empty)(q)
        assert(!result.output.value)
      }

      "return 'true' when the values are not equal" in {
        val q = const(2 + 2) !== 5
        val result = eval(FactTable.empty)(q)
        assert(result.output.value)
      }
    }
  }
}
