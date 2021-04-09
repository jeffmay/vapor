package com.rallyhealth

package vapors.dsl

import vapors.algebra.Expr
import vapors.example.{FactTypes, GenericMeasurement, Probs}
import vapors.lens.DataPath

import org.scalatest.Inside.inside
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

import java.time.Instant

class ExprBuilderSpec extends AnyWordSpec {

  "ValExprBuilder" should {

    "combine lenses from chained .get() methods" in {
      val q = factsOfType(FactTypes.GenericMeasurement).exists {
        _.get(_.select(_.value)).get(_.select(_.value)) > 0.0
      }.returnOutput
      inside(q) {
        case Expr.ExistsInOutput(_, condExpr, _) =>
          inside(condExpr) {
            case Expr.OutputWithinWindow(Expr.SelectFromOutput(_, valueLens, _), _, _) =>
              assertResult(DataPath.empty.atField("value").atField("value")) {
                valueLens.path
              }
              val fact = FactTypes.GenericMeasurement(GenericMeasurement("core/exampleample", 1.0, "m", Instant.now()))
              assertResult(fact.value.value) {
                valueLens.get(fact)
              }
          }
      }
    }

    "combine lenses from .get() and .getFoldable() methods" in {
      val q = factsOfType(FactTypes.ProbabilityToUse).exists {
        _.get(_.select(_.value)).getFoldable(_.select(_.scores)).isEmpty
      }.returnOutput
      inside(q) {
        case Expr.ExistsInOutput(_, condExpr, _) =>
          inside(condExpr) {
            case Expr.OutputIsEmpty(Expr.SelectFromOutput(_, valueLens, _), _) =>
              assertResult(DataPath.empty.atField("value").atField("scores")) {
                valueLens.path
              }
              val fact = FactTypes.ProbabilityToUse(Probs(Map()))
              assertResult(fact.value.scores) {
                valueLens.get(fact)
              }
          }
      }
    }
  }

}
