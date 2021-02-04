package com.rallyhealth.vapors.factfilter.evaluator

import com.rallyhealth.vapors.factfilter.Example.FactTypes
import com.rallyhealth.vapors.factfilter.dsl.ExprDsl._
import org.scalatest.wordspec.AnyWordSpec

class DisplayExprSpec extends AnyWordSpec {

  "DisplayExpr" when {

    "serializing with no spaces" should {

      "print a map and select from list of facts" in {
        val q = withFactsOfType(FactTypes.Age).where { facts =>
          facts.toList.map(_.get(_.select(_.value)))
        }
        assertResult {
          """withFactsOfType(FactType('age' as Int)).build(mapFrom(selectFrom(_.toList, return input), selectFrom(_.value, return input)))"""
        } {
          DisplayExpr.serialize(q)
        }
      }
    }

    "serializing with newlines and 2 spaces" should {

      "print a map and select from list of facts" in {
        val q = withFactsOfType(FactTypes.Age).where { facts =>
          facts.toList.map(_.get(_.select(_.value)))
        }
        pendingUntilFixed {
          assertResult {
            """withFactsOfType(
              |  FactType('age' as Int),
              |).build(
              |  mapFrom(
              |    selectFrom(_.toList, return input),
              |    selectFrom(_.value, return input),
              |  )
              |)""".stripMargin
          } {
            DisplayExpr.prettyPrint(q)
          }
        }
      }
    }
  }
}
