package com.rallyhealth.vapors.core.evaluator

import cats.instances.string._
import com.rallyhealth.vapors.core.data.Window
import com.rallyhealth.vapors.factfilter.Example._
import com.rallyhealth.vapors.factfilter.data.{FactTypeSet, FactsMatch}
import com.rallyhealth.vapors.factfilter.dsl._
import org.scalatest.wordspec.AnyWordSpec

final class FreeApEvaluatorSpec extends AnyWordSpec {

  import cats.instances.ordering._
  import cats.instances.order._
  import cats.instances.int._
  import cats.instances.double._

  "evaluator" should {

    "filter to the expected fact types" in {
      val q = {
        or(
          withType(FactTypes.Age).whereAnyFactValue {
            _ > 40
          },
          withType(FactTypes.WeightMeasurement).whereAnyFactValue {
            _.at(_.select(_.value)) > 300
          },
          withType(FactTypes.ProbabilityToUse).whereAnyFactValue { value =>
            value.at(_.select(_.scores).atKey("weightloss")).exists {
              _ > 0.5
            }
          },
        )
      }
      assertResult(FactsMatch(Facts(JoeSchmoe.probs))) {
        evalWithFacts(JoeSchmoe.facts)(q)
      }
    }

    "filter to the expected fact types in nested logical steps" in {
      val q =
        or(
          and(
            withTypeIn(FactTypeSets.Weight).whereAnyFactValue { value =>
              value.at(_.select(_.value)) > 200 and value.at(_.select(_.value)) <= 300
            },
            withTypeIn(FactTypeSet.of(FactTypes.Age)).whereAnyFactValue {
              _ > 45
            },
          ),
          withTypeIn(FactTypeSets.Weight).whereAnyFactValue {
            _.at(_.select(_.value)) > 300
          },
          withType(FactTypes.ProbabilityToUse).whereAnyFactValue { value =>
            value.at(_.select(_.scores).atKey("weightloss")).exists {
              _ > 0.5
            }
          },
        )
      assertResult(FactsMatch(Facts(JoeSchmoe.probs))) {
        evalWithFacts(JoeSchmoe.facts)(q)
      }
    }

    "return matching facts if any fact has prob for weightloss > existing amount" in {
      val q = {
        withType(FactTypes.ProbabilityToUse).whereAnyFactValue { value =>
          value.at(_.select(_.scores).atKey("weightloss")).exists {
            _ > 0.4
          }
        }
      }
      assertResult(FactsMatch(Facts(JoeSchmoe.probs))) {
        evalWithFacts(JoeSchmoe.facts)(q)
      }
    }

    "return matching facts if any fact within some given window" in {
      val q = {
        withTypeIn(FactTypeSets.Weight).whereAnyFactValue {
          _.at(_.select(_.value)).within(Window.between(100, 250))
        }
      }
      assertResult(FactsMatch(Facts(JoeSchmoe.weightSelfReported))) {
        evalWithFacts(JoeSchmoe.facts)(q)
      }
    }

    "return matching facts if any fact has prob for string === value" in {
      val q = {
        withType(FactTypes.Tag).whereAnyFactValue {
          _ === "asthma"
        }
      }
      assertResult(FactsMatch(Facts(JoeSchmoe.asthmaTag))) {
        evalWithFacts(JoeSchmoe.facts)(q)
      }
    }

    "return matching facts if string in a set" in {
      val q = {
        withType(FactTypes.Tag).whereAnyFactValue {
          _ in Set("asthma", "lung cancer")
        }
      }
      assertResult(FactsMatch(Facts(JoeSchmoe.asthmaTag))) {
        evalWithFacts(JoeSchmoe.facts)(q)
      }
    }

    "using whereEveryValue" in {
      val q = {
        withType(FactTypes.BloodPressureMeasurement).whereEveryFactValue { value =>
          and(
            value.at(_.select(_.diastolic)) < 150,
            value.at(_.select(_.systolic)) < 100,
          )
        }
      }
      assertResult(FactsMatch(Facts(JoeSchmoe.bloodPressure))) {
        evalWithFacts(JoeSchmoe.facts)(q)
      }
    }

    "using sealed trait / case object pattern" in {
      val q = {
        withType(FactTypes.Role).whereAnyFactValue { value =>
          value >= Role.Admin
        }
      }
      assertResult(FactsMatch(Facts(JoeSchmoe.adminRole))) {
        evalWithFacts(JoeSchmoe.facts)(q)
      }
    }
  }

}
