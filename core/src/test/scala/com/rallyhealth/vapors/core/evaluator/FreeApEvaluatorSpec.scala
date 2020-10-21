package com.rallyhealth.vapors.core.evaluator

import cats.instances.string._
import com.rallyhealth.vapors.core.data.Window
import com.rallyhealth.vapors.factfilter.Example._
import com.rallyhealth.vapors.factfilter.data.{FactTypeSet, FactsMatch}
import com.rallyhealth.vapors.factfilter.dsl._
import org.scalatest.wordspec.AnyWordSpec

final class FreeApEvaluatorSpec extends AnyWordSpec {

  "evaluator" should {

    "filter to the expected fact types" in {
      val q = {
        or(
          withType(FactTypes.Age).where {
            _.value.exists {
              _ > 40
            }
          },
          withType(FactTypes.WeightMeasurement).where {
            _.value.exists {
              _ > 300
            }
          },
          withType(FactTypes.ProbabilityToUse).where {
            _.value.exists {
              _.withValueAt(_.select(_.scores).atKey("weightloss")) {
                _.exists {
                  _ > 0.5
                }
              }
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
            withTypeIn(FactTypeSets.Weight).where {
              _.value.exists { value =>
                value > 200 and value <= 300
              }
            },
            withTypeIn(FactTypeSet.of(FactTypes.Age)).where {
              _.value.exists {
                _ > 45
              }
            },
          ),
          withTypeIn(FactTypeSets.Weight).where {
            _.value.exists {
              _ > 300
            }
          },
          withType(FactTypes.ProbabilityToUse).where {
            _.value.exists {
              _.withValueAt(_.select(_.scores).atKey("weightloss")) {
                _.exists {
                  _ > 0.5
                }
              }
            }
          },
        )
      assertResult(FactsMatch(Facts(JoeSchmoe.probs))) {
        evalWithFacts(JoeSchmoe.facts)(q)
      }
    }

    "return matching facts if any fact has prob for weightloss > existing amount" in {
      val q = {
        withType(FactTypes.ProbabilityToUse).where {
          _.value.exists {
            _.withValueAt(_.select(_.scores).atKey("weightloss")) {
              _.exists(_ > 0.4)
            }
          }
        }
      }
      assertResult(FactsMatch(Facts(JoeSchmoe.probs))) {
        evalWithFacts(JoeSchmoe.facts)(q)
      }
    }

    "return matching facts if any fact within some given window" in {
      val q = {
        withTypeIn(FactTypeSets.Weight).where {
          _.value.exists {
            _.within(Window.between(100, 250))
          }
        }
      }
      assertResult(FactsMatch(Facts(JoeSchmoe.weightSelfReported))) {
        evalWithFacts(JoeSchmoe.facts)(q)
      }
    }

    "return matching facts if any fact has prob for string === value" in {
      val q = {
        withType(FactTypes.Tag).where {
          _.value.exists {
            _ === "asthma"
          }
        }
      }
      assertResult(FactsMatch(Facts(JoeSchmoe.asthmaTag))) {
        evalWithFacts(JoeSchmoe.facts)(q)
      }
    }

    "return matching facts if string in a set" in {
      val q = {
        withType(FactTypes.Tag).where {
          _.value.exists {
            _ in Set("asthma", "lung cancer")
          }
        }
      }
      assertResult(FactsMatch(Facts(JoeSchmoe.asthmaTag))) {
        evalWithFacts(JoeSchmoe.facts)(q)
      }
    }

    "using whereEveryValue" in {
      val q = {
        withType(FactTypes.BloodPressureMeasurement).where {
          _.value.exists { value =>
            and(
              value.at(_.select(_.diastolic)) < 150,
              value.at(_.select(_.systolic)) < 100,
            )
          }
        }
      }
      assertResult(FactsMatch(Facts(JoeSchmoe.bloodPressure))) {
        evalWithFacts(JoeSchmoe.facts)(q)
      }
    }

    "using sealed trait / case object pattern" in {
      val q = {
        withType(FactTypes.Role).where {
          _.value.exists {
            _ >= Role.Admin
          }
        }
      }
      assertResult(FactsMatch(Facts(JoeSchmoe.adminRole))) {
        evalWithFacts(JoeSchmoe.facts)(q)
      }
    }
  }

}
