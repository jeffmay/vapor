package com.rallyhealth

package vapors.interpreter

import vapors.data.{DerivedFactOfType, Evidence, FactTable}
import vapors.dsl._
import vapors.example.{FactTypes, JoeSchmoe, Role, Snippets}

import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

import java.time.LocalDate

class UsingDefinitionsExprSpec extends AnyWordSpec {

  "Expr.UsingDefinitions" should {

    "add the defined facts to the fact table" in {
      val result = eval(JoeSchmoe.factTable) {
        usingDefinitions(Snippets.ageFromDateOfBirthDef) {
          valuesOfType(FactTypes.Age)
        }
      }
      val ages = result.output.value
      ages should contain only JoeSchmoe.age.value
    }

    "return the result of an expression with the Definition-produced facts as DerivedFacts" in {
      val yearWhen19 = LocalDate.now().getYear - 19
      val expiredAge = FactTypes.Age(17)
      val dob = FactTypes.DateOfBirth(LocalDate.of(yearWhen19, 1, 1))
      val facts = FactTable(expiredAge, dob)
      val result = eval(facts)(Snippets.isOver18)
      assert(result.output.value)
      val ageFromYear = LocalDate.now().getYear - dob.value.getYear
      val expectedDerivedFact = DerivedFactOfType(FactTypes.Age, ageFromYear, Evidence(dob))
      assertResult(Evidence(expectedDerivedFact))(result.output.evidence)
      assertResult(Evidence(dob))(result.output.evidence.derivedFromSources)
    }

    "support nested fact definitions" in {
      val yearWhen19 = LocalDate.now().getYear - 19
      val userRole = FactTypes.Role(Role.User)
      val expiredAge = FactTypes.Age(17)
      val dob = FactTypes.DateOfBirth(LocalDate.of(yearWhen19, 1, 1))
      val facts = FactTable(expiredAge, dob, userRole)
      val definition = Snippets.isEligibleDef
      val result = eval(facts) {
        usingDefinitions(definition) {
          factsOfType(definition.factType).exists {
            _.get(_.select(_.value))
          }
        }
      }
      assert(result.output.value)
      val correctAge = DerivedFactOfType(FactTypes.Age, 19, Evidence(dob))
      val evidenceOfEligibility = Evidence(
        DerivedFactOfType(definition.factType, true, Evidence(correctAge, userRole)),
      )
      assertResult(evidenceOfEligibility)(result.output.evidence)
      assertResult(Evidence(dob, userRole))(result.output.evidence.derivedFromSources)
    }

  }
}
