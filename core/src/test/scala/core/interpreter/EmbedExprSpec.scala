package com.rallyhealth.vapors

package core.interpreter

import core.data._
import core.dsl._
import core.example.{FactTypes, JoeSchmoe}

import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class EmbedExprSpec extends AnyWordSpec {

  "embedding an expression inside a logical operator inside a 'withFactsOfType'" when {

    def insideProbOfWeightloss(cond: ValCondExpr[Double, Unit]): RootExpr[Boolean, Unit] = {
      factsOfType(FactTypes.ProbabilityToUse)
        .flatMap {
          _.getFoldable {
            _.select(_.value).select(_.scores).at("weightloss").to(Seq)
          }
        }
        .exists { _ =>
          cond
        }
    }

    def weightMeasuredWithin(window: Window[Int]): RootExpr[Boolean, Unit] = {
      import cats.syntax.invariant._
      val doubleWindow = window.imap(_.toDouble)(_.toInt)
      factsOfType(FactTypes.WeightMeasurement).exists {
        _.get(_.select(_.value).select(_.value)).within(doubleWindow)
      }
    }

    val trueEmbedded = weightMeasuredWithin(Window.greaterThan(200))
    val falseEmbedded = weightMeasuredWithin(Window.greaterThan(300))
    val embeddedFacts = FactSet(JoeSchmoe.weight)

    def trueLiteral: ValCondExpr[Double, Unit] = {
      within(input, Window.greaterThan(0.7))
    }

    def falseLiteral: ValCondExpr[Double, Unit] = {
      within(input, Window.greaterThan(0.9))
    }

    val literalFacts = FactSet(JoeSchmoe.probs)

    "inside an 'or'" should {

      "return 'true' when embedding a 'true' expression BEFORE a 'true' literal" in {
        val q = insideProbOfWeightloss(or(trueEmbedded, trueLiteral))
        val result = eval(JoeSchmoe.factTable)(q)
        assert(result.output.value)
        assertResult(Evidence(embeddedFacts | literalFacts))(result.output.evidence)
      }

      "return 'true' when embedding a 'true' expression BEFORE a 'false' literal" in {
        val q = insideProbOfWeightloss(or(trueEmbedded, falseLiteral))
        val result = eval(JoeSchmoe.factTable)(q)
        assert(result.output.value)
        assertResult(Evidence(embeddedFacts))(result.output.evidence)
      }

      "return 'true' when embedding a 'false' expression BEFORE a 'true' literal" in {
        val q = insideProbOfWeightloss(or(falseEmbedded, trueLiteral))
        val result = eval(JoeSchmoe.factTable)(q)
        assert(result.output.value)
        assertResult(Evidence(literalFacts))(result.output.evidence)
      }

      "return 'false' when embedding a 'false' expression BEFORE a 'false' literal" in {
        val q = insideProbOfWeightloss(or(falseEmbedded, falseLiteral))
        val result = eval(JoeSchmoe.factTable)(q)
        assert(!result.output.value)
        assertResult(Evidence.none)(result.output.evidence)
      }

      "return 'true' when embedding a 'true' expression AFTER a 'true' literal" in {
        val q = insideProbOfWeightloss(or(trueLiteral, trueEmbedded))
        val result = eval(JoeSchmoe.factTable)(q)
        assert(result.output.value)
        assertResult(Evidence(literalFacts | embeddedFacts))(result.output.evidence)
      }

      "return 'true' when embedding a 'true' expression AFTER a 'false' literal" in {
        val q = insideProbOfWeightloss(or(falseLiteral, trueEmbedded))
        val result = eval(JoeSchmoe.factTable)(q)
        assert(result.output.value)
        assertResult(Evidence(embeddedFacts))(result.output.evidence)
      }

      "return 'true' when embedding a 'false' expression AFTER a 'true' literal" in {
        val q = insideProbOfWeightloss(or(trueLiteral, falseEmbedded))
        val result = eval(JoeSchmoe.factTable)(q)
        assert(result.output.value)
        assertResult(Evidence(literalFacts))(result.output.evidence)
      }

      "return 'false' when embedding a 'false' expression AFTER a 'false' literal" in {
        val q = insideProbOfWeightloss(or(falseLiteral, falseEmbedded))
        val result = eval(JoeSchmoe.factTable)(q)
        assert(!result.output.value)
        assertResult(Evidence.none)(result.output.evidence)
      }

      "disallows embedding an invalid return type" in {
        val listOfNumberExpr = factsOfType(FactTypes.ProbabilityToUse).flatMap {
          _.getFoldable(_.select(_.value).select(_.scores).at("weightloss").to(Seq))
        }
        assertDoesNotCompile {
          """insideProbOfWeightloss(or(trueLiteral, listOfNumberExpr))"""
        }
      }
    }

    "inside an 'and'" should {

      "return 'true' when embedding a 'true' expression BEFORE a 'true' literal" in {
        val q = insideProbOfWeightloss(and(trueEmbedded, trueLiteral))
        val result = eval(JoeSchmoe.factTable)(q)
        assert(result.output.value)
        assertResult(Evidence(embeddedFacts | literalFacts))(result.output.evidence)
      }

      "return 'false' when embedding a 'true' expression BEFORE a 'false' literal" in {
        val q = insideProbOfWeightloss(and(trueEmbedded, falseLiteral))
        val result = eval(JoeSchmoe.factTable)(q)
        assert(!result.output.value)
        assertResult(Evidence.none)(result.output.evidence)
      }

      "return 'false' when embedding a 'false' expression BEFORE a 'true' literal" in {
        val q = insideProbOfWeightloss(and(falseEmbedded, trueLiteral))
        val result = eval(JoeSchmoe.factTable)(q)
        assert(!result.output.value)
        assertResult(Evidence.none)(result.output.evidence)
      }

      "return 'false' when embedding a 'false' expression BEFORE a 'false' literal" in {
        val q = insideProbOfWeightloss(and(falseEmbedded, falseLiteral))
        val result = eval(JoeSchmoe.factTable)(q)
        assert(!result.output.value)
        assertResult(Evidence.none)(result.output.evidence)
      }

      "return 'true' when embedding a 'true' expression AFTER a 'true' literal" in {
        val q = insideProbOfWeightloss(and(trueLiteral, trueEmbedded))
        val result = eval(JoeSchmoe.factTable)(q)
        assert(result.output.value)
        assertResult(Evidence(literalFacts | embeddedFacts))(result.output.evidence)
      }

      "return 'false' when embedding a 'true' expression AFTER a 'false' literal" in {
        val q = insideProbOfWeightloss(and(falseLiteral, trueEmbedded))
        val result = eval(JoeSchmoe.factTable)(q)
        assert(!result.output.value)
        assertResult(Evidence.none)(result.output.evidence)
      }

      "return 'false' when embedding a 'false' expression AFTER a 'true' literal" in {
        val q = insideProbOfWeightloss(and(trueLiteral, falseEmbedded))
        val result = eval(JoeSchmoe.factTable)(q)
        assert(!result.output.value)
        assertResult(Evidence.none)(result.output.evidence)
      }

      "return 'false' when embedding a 'false' expression AFTER a 'false' literal" in {
        val q = insideProbOfWeightloss(and(falseLiteral, falseEmbedded))
        val result = eval(JoeSchmoe.factTable)(q)
        assert(!result.output.value)
        assertResult(Evidence.none)(result.output.evidence)
      }

      "disallows embedding an invalid return type" in {
        val listOfNumberExpr = factsOfType(FactTypes.ProbabilityToUse).flatMap {
          _.getFoldable(_.select(_.value).select(_.scores).at("weightloss").asIterable.to(Seq))
        }
        assertDoesNotCompile {
          """insideProbOfWeightloss(and(trueLiteral, listOfNumberExpr))"""
        }
      }
    }
  }
}
