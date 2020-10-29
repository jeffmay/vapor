package com.rallyhealth.vapors.factfilter.evaluator

import cats.Id
import com.rallyhealth.vapors.core.algebra.Expr
import com.rallyhealth.vapors.core.data.Window
import com.rallyhealth.vapors.factfilter.Example.{FactTypes, JoeSchmoe}
import com.rallyhealth.vapors.factfilter.data._
import com.rallyhealth.vapors.factfilter.dsl.ExprDsl._
import com.rallyhealth.vapors.factfilter.dsl.Facts
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

import java.time.LocalDate

class EmbeddedExpressionSpec extends AnyWordSpec {

  import cats.instances.all._
  import com.rallyhealth.vapors.factfilter.dsl.CaptureP.unit._

  // TODO: Do the real computation here
  // TODO: Make a named function to call out to "dateCompare()" (maybe a primitive algebra?)
  //       Or just hack it for now.
  // now.year - dob.year - (if (now.dayOfYear < (dob.dayOfYear + (if (dob.isLeapYear) 1 else 0)) 1 else 0)
  lazy val ageFromDateOfBirth: Expr.Definition[Unit] = {
    val now = LocalDate.now()
    define(FactTypes.Age) {
      withFactsOfType(FactTypes.DateOfBirth).where { facts =>
        facts.map { fact =>
          fact.get(_.select(_.value).select(_.getYear)).subtractFrom(now.getYear)
        }
      }
    }
  }

  lazy val isOver18: RootExpr[Boolean, Unit] = {
    usingDefinitions(ageFromDateOfBirth) {
      withFactsOfType(FactTypes.Age).where {
        _.exists {
          _.get(_.select(_.value)) >= 18
        }
      }
    }
  }

  "compute age correctly" in {
    val result = eval(JoeSchmoe.factTable) {
      usingDefinitions(ageFromDateOfBirth) {
        withFactsOfType(FactTypes.Age).returnInput
      }
    }
    val ages = result.output.value.map(_.value)
    ages should contain only JoeSchmoe.age.value
  }

  "overwrite another fact type" in {
    val dob = FactTypes.DateOfBirth(LocalDate.of(1990, 1, 1))
    val facts = FactTable(
      List(
        FactTypes.Age(15),
        dob,
      ),
    )
    val result = eval(facts)(isOver18)
    assert(result.output.value)
    pendingUntilFixed {
      assertResult(FactsMatch(Facts(dob)))(result.output.evidence)
    }
  }

  "embedding an expression inside a logical operator inside a 'withFactsOfType'" when {

    def insideProbOfWeightloss(cond: Expr[Id, Double, Boolean, Unit]): RootExpr[Boolean, Unit] = {
      withFactsOfType(FactTypes.ProbabilityToUse).where {
        _.flatMap {
          _.getFoldable[List, Double] {
            _.select(_.value).select(_.scores).atKey("weightloss").to(List)
          }
        }.exists {
          _.embed(cond)
        }
      }
    }

    def weightMeasuredWithin(window: Window[Int]): RootExpr[Boolean, Unit] = {
      import cats.syntax.invariant._
      val doubleWindow = window.imap(_.toDouble)(_.toInt)
      withFactsOfType(FactTypes.WeightMeasurement).where {
        _.exists {
          _.get(_.select(_.value).select(_.value)).within(doubleWindow)
        }
      }
    }

    val trueEmbedded = weightMeasuredWithin(Window.greaterThan(200))
    val falseEmbedded = weightMeasuredWithin(Window.greaterThan(300))
    val embeddedFacts = Facts(JoeSchmoe.weight)

    def trueLiteral: Expr[Id, Double, Boolean, Unit] = {
      within(input, Window.greaterThan(0.7))
    }

    def falseLiteral: Expr[Id, Double, Boolean, Unit] = {
      within(input, Window.greaterThan(0.9))
    }

    val literalFacts = Facts(JoeSchmoe.probs)

    "inside an 'or'" should {

      "return 'true' when embedding a 'true' expression BEFORE a 'true' literal" in {
        val q = insideProbOfWeightloss(or(trueEmbedded, trueLiteral))
        val result = eval(JoeSchmoe.factTable)(q)
        assert(result.output.value)
        assertResult(FactsMatch(embeddedFacts ::: literalFacts))(result.output.evidence)
      }

      "return 'true' when embedding a 'true' expression BEFORE a 'false' literal" in {
        val q = insideProbOfWeightloss(or(trueEmbedded, falseLiteral))
        val result = eval(JoeSchmoe.factTable)(q)
        assert(result.output.value)
        assertResult(FactsMatch(embeddedFacts))(result.output.evidence)
      }

      "return 'true' when embedding a 'false' expression BEFORE a 'true' literal" in {
        val q = insideProbOfWeightloss(or(falseEmbedded, trueLiteral))
        val result = eval(JoeSchmoe.factTable)(q)
        assert(result.output.value)
        assertResult(FactsMatch(literalFacts))(result.output.evidence)
      }

      "return 'false' when embedding a 'false' expression BEFORE a 'false' literal" in {
        val q = insideProbOfWeightloss(or(falseEmbedded, falseLiteral))
        val result = eval(JoeSchmoe.factTable)(q)
        assert(!result.output.value)
        assertResult(NoFactsMatch())(result.output.evidence)
      }

      "return 'true' when embedding a 'true' expression AFTER a 'true' literal" in {
        val q = insideProbOfWeightloss(or(trueLiteral, trueEmbedded))
        val result = eval(JoeSchmoe.factTable)(q)
        assert(result.output.value)
        assertResult(FactsMatch(literalFacts ::: embeddedFacts))(result.output.evidence)
      }

      "return 'true' when embedding a 'true' expression AFTER a 'false' literal" in {
        val q = insideProbOfWeightloss(or(falseLiteral, trueEmbedded))
        val result = eval(JoeSchmoe.factTable)(q)
        assert(result.output.value)
        assertResult(FactsMatch(embeddedFacts))(result.output.evidence)
      }

      "return 'true' when embedding a 'false' expression AFTER a 'true' literal" in {
        val q = insideProbOfWeightloss(or(trueLiteral, falseEmbedded))
        val result = eval(JoeSchmoe.factTable)(q)
        assert(result.output.value)
        assertResult(FactsMatch(literalFacts))(result.output.evidence)
      }

      "return 'false' when embedding a 'false' expression AFTER a 'false' literal" in {
        val q = insideProbOfWeightloss(or(falseLiteral, falseEmbedded))
        val result = eval(JoeSchmoe.factTable)(q)
        assert(!result.output.value)
        assertResult(NoFactsMatch())(result.output.evidence)
      }

      "disallows embedding an invalid return type" in {
        val listOfNumberExpr = withFactsOfType(FactTypes.ProbabilityToUse).where {
          _.flatMap {
            _.getFoldable(_.select(_.value).select(_.scores).atKey("weightloss").to(List))
          }
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
        assertResult(FactsMatch(embeddedFacts ::: literalFacts))(result.output.evidence)
      }

      "return 'false' when embedding a 'true' expression BEFORE a 'false' literal" in {
        val q = insideProbOfWeightloss(and(trueEmbedded, falseLiteral))
        val result = eval(JoeSchmoe.factTable)(q)
        assert(!result.output.value)
        assertResult(NoFactsMatch())(result.output.evidence)
      }

      "return 'false' when embedding a 'false' expression BEFORE a 'true' literal" in {
        val q = insideProbOfWeightloss(and(falseEmbedded, trueLiteral))
        val result = eval(JoeSchmoe.factTable)(q)
        assert(!result.output.value)
        assertResult(NoFactsMatch())(result.output.evidence)
      }

      "return 'false' when embedding a 'false' expression BEFORE a 'false' literal" in {
        val q = insideProbOfWeightloss(and(falseEmbedded, falseLiteral))
        val result = eval(JoeSchmoe.factTable)(q)
        assert(!result.output.value)
        assertResult(NoFactsMatch())(result.output.evidence)
      }

      "return 'true' when embedding a 'true' expression AFTER a 'true' literal" in {
        val q = insideProbOfWeightloss(and(trueLiteral, trueEmbedded))
        val result = eval(JoeSchmoe.factTable)(q)
        assert(result.output.value)
        assertResult(FactsMatch(literalFacts ::: embeddedFacts))(result.output.evidence)
      }

      "return 'false' when embedding a 'true' expression AFTER a 'false' literal" in {
        val q = insideProbOfWeightloss(and(falseLiteral, trueEmbedded))
        val result = eval(JoeSchmoe.factTable)(q)
        assert(!result.output.value)
        assertResult(NoFactsMatch())(result.output.evidence)
      }

      "return 'false' when embedding a 'false' expression AFTER a 'true' literal" in {
        val q = insideProbOfWeightloss(and(trueLiteral, falseEmbedded))
        val result = eval(JoeSchmoe.factTable)(q)
        assert(!result.output.value)
        assertResult(NoFactsMatch())(result.output.evidence)
      }

      "return 'false' when embedding a 'false' expression AFTER a 'false' literal" in {
        val q = insideProbOfWeightloss(and(falseLiteral, falseEmbedded))
        val result = eval(JoeSchmoe.factTable)(q)
        assert(!result.output.value)
        assertResult(NoFactsMatch())(result.output.evidence)
      }

      "disallows embedding an invalid return type" in {
        val listOfNumberExpr = withFactsOfType(FactTypes.ProbabilityToUse).where {
          _.flatMap {
            _.getFoldable(_.select(_.value).select(_.scores).atKey("weightloss").to(List))
          }
        }
        assertDoesNotCompile {
          """insideProbOfWeightloss(and(trueLiteral, listOfNumberExpr))"""
        }
      }
    }
  }
}
