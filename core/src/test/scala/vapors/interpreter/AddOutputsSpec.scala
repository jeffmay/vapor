package com.rallyhealth

package vapors.interpreter

import vapors.dsl._

import org.scalatest.freespec.AnyFreeSpec

class AddOutputsSpec extends AnyFreeSpec {

  "Expr.AddOutputs" - {

    "Int" - {

      "expression added to an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[Int, Int, Int, ArithmeticException](
          _ + _,
          const(_) + const(_),
        )
      }

      "value added to an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[Int, Int, Int, ArithmeticException](
          _ + _,
          const(_) + _,
        )
      }

      "expression added to a value" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[Int, Int, Int, ArithmeticException](
          _ + _,
          const(_).addTo(_),
        )
      }
    }

    "Double" - {

      "expression added to an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[Double, Double, Double, ArithmeticException](
          _ + _,
          const(_) + const(_),
        )
      }

      "value added to an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[Double, Double, Double, ArithmeticException](
          _ + _,
          const(_) + _,
        )
      }

      "expression added to a value" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[Double, Double, Double, ArithmeticException](
          _ + _,
          const(_).addTo(_),
        )
      }
    }
  }
}
