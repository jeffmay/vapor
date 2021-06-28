package com.rallyhealth

package vapors.interpreter

import vapors.data.FactTable
import vapors.dsl._

import org.scalatest.wordspec.AnyWordSpec

class WhenExprSpec extends AnyWordSpec {

  // a set of ordered constants for comparison
  private final val a = "A"
  private final val b = "B"
  private final val c = "C"
  private final val d = "D"

  "Expr.When" when {

    "operating on constants" should {

      "return the first branch of an if / else when the first condition is 'true'" in {
        val q = when(const(true)).thenReturn(const(a)).elseReturn(const(b))
        val res = eval(FactTable.empty)(q)
        assertResult(a)(res.output.value)
      }

      "return the second branch of an if / else when the first condition is 'false'" in {
        val q = when(const(false)).thenReturn(const(a)).elseReturn(const(b))
        val res = eval(FactTable.empty)(q)
        assertResult(b)(res.output.value)
      }

      "return the first branch of a if / elif / else when the first condition is 'true' and the second condition is 'false'" in {
        val q = when(const(true))
          .thenReturn(const(a))
          .elif(const(false))
          .thenReturn(const(b))
          .elseReturn(const(c))
        val res = eval(FactTable.empty)(q)
        assertResult(a)(res.output.value)
      }

      "return the first branch of a if / elif / else when the first condition is 'true' and the second condition is 'true'" in {
        val q = when(const(true))
          .thenReturn(const(a))
          .elif(const(true))
          .thenReturn(const(b))
          .elseReturn(const(c))
        val res = eval(FactTable.empty)(q)
        assertResult(a)(res.output.value)
      }

      "return the second branch of a if / elif / else when the first condition is 'false' and the second condition is 'true'" in {
        val q = when(const(false))
          .thenReturn(const(a))
          .elif(const(true))
          .thenReturn(const(b))
          .elseReturn(const(c))
        val res = eval(FactTable.empty)(q)
        assertResult(b)(res.output.value)
      }

      "return the last branch of a if / elif / else when all conditions are 'false'" in {
        val q = when(const(false))
          .thenReturn(const(a))
          .elif(const(false))
          .thenReturn(const(b))
          .elseReturn(const(c))
        val res = eval(FactTable.empty)(q)
        assertResult(c)(res.output.value)
      }

      "return the third branch of a if / elif / elif / else when the first two conditions are 'false' and the third condition is 'true'" in {
        val q = when(const(false))
          .thenReturn(const(a))
          .elif(const(false))
          .thenReturn(const(b))
          .elif(const(true))
          .thenReturn(const(c))
          .elseReturn(const(d))
        val res = eval(FactTable.empty)(q)
        assertResult(c)(res.output.value)
      }
    }
  }
}
