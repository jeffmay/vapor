package com.rallyhealth.vapors.core.fql

import cats.data.NonEmptyList
import com.rallyhealth.vapors.core.Example._
import com.rallyhealth.vapors.core.data.FactsMatch
import com.rallyhealth.vapors.core.dsl._
import com.rallyhealth.vapors.core.evaluator._
import org.scalatest.wordspec.AnyWordSpec

class PrinterSpec extends AnyWordSpec {

  "Printer.serialize" should {

    "print an expression that selects a specific fact type using whereValue(_ > 30)" in {
      val q = {
        __.withFactsOfType(FactTypes.age)
          .whereAnyValue(__ > 30)
      }
      assertResult(FactsMatch(NonEmptyList.of(JoeSchmoe.age))) {
        eval(JoeSchmoe.facts)(q)
      }
      assertResult(
        "_.collect { case f: Fact[Int] => f.exists(_.value where x > 30) }",
      ) {
        printer.serialize(q)
      }
    }

    "print an expression that selects a fact type using where(factValue(_ > 200))" in {
      val q = {
        __.withFactsOfType(FactTypes.weight)
          .whereAnyValue(__ > 200)
      }
      assertResult(FactsMatch(NonEmptyList.of(JoeSchmoe.weight))) {
        eval(JoeSchmoe.facts)(q)
      }
      assertResult(
        "_.collect { case f: Fact[Int] => f.exists(_.value where x > 200) }",
      ) {
        printer.serialize(q)
      }
    }

    "print an expression that selects a value using the NamedLens select" in {
      val q = {
        __.withFactsOfType(FactTypes.probs)
          .withValuesAt(_.select(_.scores).atKey("weightloss"))
          .whereAnyValue(
            __.or(
              __.exists(__ > 0.5),
              __.exists(__ < 0.3),
            ),
          )
      }
      assertResult(FactsMatch(NonEmptyList.of(JoeSchmoe.probs))) {
        eval(JoeSchmoe.facts)(q)
      }
      assertResult(
        // TODO: This query looks wrong...
        //       there is a mismatch between how value selection works within .exists() and conditional expressions
        "_.collect {" +
          " case f: Fact[Example.Probs] =>" +
          " f.exists(_.value.scores['weightloss'] .exists(_where x > 0.5) or .exists(_where x < 0.3)) }",
      ) {
        printer.serialize(q)
      }
    }
  }

}
