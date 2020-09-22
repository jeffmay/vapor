package com.rallyhealth.vapors.core.evaluator

import cats.data.NonEmptyList
import com.rallyhealth.vapors.core.Example._
import com.rallyhealth.vapors.core.data.FactsMatch
import com.rallyhealth.vapors.core.dsl
import org.scalatest.wordspec.AnyWordSpec

final class FreeApEvaluatorSpec extends AnyWordSpec {
  import dsl._

  // Fact specific tests

  "evaluator" should {

    "filter to the expected fact types in nested logical steps" in {
      val q = {
        or(
          __.withFactsOfType(FactTypes.age)
            .whereAnyValue(__ > 300),
          __.withFactsOfType(FactTypes.weight)
            .whereAnyValue(__ > 300),
          __.withFactsOfType(FactTypes.probs)
            .withValuesAt(_.select(_.scores).atKey("weightloss"))
            .whereAnyValue {
              __.exists(__ > 0.5)
            },
        )
      }
      assertResult(FactsMatch(NonEmptyList.of(JoeSchmoe.probs))) {
        eval(JoeSchmoe.facts)(q)
      }
    }

    "return matching facts if any fact has prob for weightloss > existing amount" in {
      val q = {
        __.withFactsOfType(FactTypes.probs)
          .withValuesAt(_.select(_.scores).atKey("weightloss"))
          .whereAnyValue {
            exists(__ > 0.4)
          }
      }
      assertResult(FactsMatch(NonEmptyList.of(JoeSchmoe.probs))) {
        eval(JoeSchmoe.facts)(q)
      }
    }
  }

}
