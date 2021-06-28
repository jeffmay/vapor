package com.rallyhealth

package vapors.lens

import vapors.data.TypedFact
import vapors.example.{AddressUpdate, BloodPressure, JoeSchmoe}

import cats.data.Chain
import org.scalatest.wordspec.AnyWordSpec
import shapeless.{::, HNil, Nat}

import java.time.LocalDate

class NamedLensSpec extends AnyWordSpec {

  "NamedLens.at" should {

    "select a value from an HList using a Nat literal" in {
      val lens = NamedLens.id[Double :: Int :: HNil].at(Nat._1)
      val expected = 1
      assertResult(expected) {
        lens.get(2.0 :: expected :: HNil)
      }
    }

    "fail to compile when using an index out of range" in {
      assertDoesNotCompile {
        "NamedLens.id[Double :: Int :: HNil].at(Nat._2)"
      }
    }
  }

  "NamedLens.select" should {

    "select a field of a case class" in {
      val lens = NamedLens.id[BloodPressure].select(_.diastolic)
      assertResult(DataPath(Chain(DataPath.Field("diastolic"))))(lens.path)
      assertResult(JoeSchmoe.bloodPressure.value.diastolic) {
        lens.get(JoeSchmoe.bloodPressure.value)
      }
    }

    "select a field of a case class within another case class" in {
      val lens = NamedLens.id[AddressUpdate].select(_.address.zip)
      assertResult(DataPath(Chain(DataPath.Field("address"), DataPath.Field("zip"))))(lens.path)
      assertResult(JoeSchmoe.lastAddressUpdate.value.address.zip) {
        lens.get(JoeSchmoe.lastAddressUpdate.value)
      }
    }

    "select a field of a case class inside of a Fact" in {
      val lens = NamedLens.id[TypedFact[BloodPressure]].select(_.value.diastolic)
      assertResult(DataPath(Chain(DataPath.Field("value"), DataPath.Field("diastolic"))))(lens.path)
      assertResult(JoeSchmoe.bloodPressure.value.diastolic) {
        lens.get(JoeSchmoe.bloodPressure)
      }
    }

    "select a field of a case class within another case class inside of a fact" in {
      val lens = NamedLens.id[TypedFact[AddressUpdate]].select(_.value.address.zip)
      assertResult(DataPath(Chain(DataPath.Field("value"), DataPath.Field("address"), DataPath.Field("zip"))))(
        lens.path,
      )
      assertResult(JoeSchmoe.lastAddressUpdate.value.address.zip) {
        lens.get(JoeSchmoe.lastAddressUpdate)
      }
    }

    "select a Java bean style 'get' method with the prefix removed" in {
      val lens = NamedLens.id[LocalDate].select(_.getYear())
      assertResult(DataPath(Chain(DataPath.Field("year"))))(lens.path)
      assertResult(JoeSchmoe.dateOfBirth.value.getYear) {
        lens.get(JoeSchmoe.dateOfBirth.value)
      }
    }

    "select a Java bean style 'is' method without removing the prefix" in {
      val lens = NamedLens.id[LocalDate].select(_.isLeapYear())
      assertResult(DataPath(Chain(DataPath.Field("isLeapYear"))))(lens.path)
      assertResult(JoeSchmoe.dateOfBirth.value.isLeapYear) {
        lens.get(JoeSchmoe.dateOfBirth.value)
      }
    }
  }
}
