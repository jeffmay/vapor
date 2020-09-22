package com.rallyhealth.vapors.core.evaluator

import com.rallyhealth.vapors.core.data._
import com.rallyhealth.vapors.core.dsl.AnyExp
import com.rallyhealth.vapors.core.logic.{Intersect, Union}
import org.scalatest.wordspec.AnyWordSpec

import scala.language.existentials

final class LogicalOperatorSpec extends AnyWordSpec {

  type LogicOpBuilder[T, A] = (AnyExp[T, A], AnyExp[T, A], Seq[AnyExp[T, A]]) => AnyExp[T, A]

  private def validLogicalOperators[T, A : Intersect : Union](
    andBuilder: LogicOpBuilder[T, A],
    orBuilder: LogicOpBuilder[T, A],
    trueBuilder: AnyExp[T, A],
    falseBuilder: AnyExp[T, A],
    input: T,
    trueOutput: A,
    falseOutput: A,
  ): Unit = {

    val T = trueBuilder
    val F = falseBuilder

    def and(
      one: AnyExp[T, A],
      two: AnyExp[T, A],
      tail: AnyExp[T, A]*,
    ): AnyExp[T, A] = andBuilder(one, two, tail)

    def or(
      one: AnyExp[T, A],
      two: AnyExp[T, A],
      tail: AnyExp[T, A]*,
    ): AnyExp[T, A] = orBuilder(one, two, tail)

    "return true for one true and one false in an or" in {
      val q = {
        or(T, F)
      }
      assertResult(trueOutput) {
        eval(input)(q)
      }
    }

    "return true for a complex or" in {
      val q = {
        or(F, F, T, F)
      }
      assertResult(trueOutput) {
        eval(input)(q)
      }
    }

    "return false for a long or" in {
      val q = {
        or(F, F, F, F)
      }
      assertResult(falseOutput) {
        eval(input)(q)
      }
    }

    "return false for a true and false in an and" in {
      val q = {
        and(T, F)
      }
      assertResult(falseOutput) {
        eval(input)(q)
      }
    }

    "return true for two trues in an and" in {
      val q = {
        and(T, T)
      }
      assertResult(trueOutput) {
        eval(input)(q)
      }
    }

    "return false for a complex and" in {
      val q = {
        and(T, T, F, T)
      }
      assertResult(falseOutput) {
        eval(input)(q)
      }
    }

    "return true for a long and" in {
      val q = {
        and(T, T, T, T)
      }
      assertResult(trueOutput) {
        eval(input)(q)
      }
    }

    "return true for nested true 'or's in an and" in {
      val q = {
        and(or(T, T), or(T, T))
      }
      assertResult(trueOutput) {
        eval(input)(q)
      }
    }

    "return false for a nested false 'or' before some true expressions" in {
      val q = {
        and(or(F, F), T, T)
      }
      assertResult(falseOutput) {
        eval(input)(q)
      }
    }

    "return false for a nested false 'or' after some true expressions" in {
      val q = {
        and(T, or(T, F), or(F, F))
      }
      assertResult(falseOutput) {
        eval(input)(q)
      }
    }

    "return true for a nested false and in an 'or'" in {
      val q = {
        or(F, and(F, F), and(T, T), and(T, F))
      }
      assertResult(trueOutput) {
        eval(input)(q)
      }
    }

    "return true for a complex structure" in {
      val q = {
        or(F, and(F, T), or(F, F), and(or(T, F), F), or(and(T, F), T))
      }
      assertResult(trueOutput) {
        eval(input)(q)
      }
    }
  }

  private abstract class DslLogicOpBuilder {

    def andBuilder[T, A : Intersect]: LogicOpBuilder[T, A]

    def orBuilder[T, A : Union]: LogicOpBuilder[T, A]
  }

  def validDslLogicalOperators(builder: DslLogicOpBuilder): Unit = {
    import com.rallyhealth.vapors.core.Example.JoeSchmoe
    import com.rallyhealth.vapors.core.dsl._

    "operating on boolean results" should {

      behave like validLogicalOperators[Unit, Boolean](
        builder.andBuilder,
        builder.orBuilder,
        trueBuilder = alwaysTrue,
        falseBuilder = alwaysFalse,
        input = (),
        trueOutput = true,
        falseOutput = false,
      )
    }

    "operating on ResultSets" should {

      behave like validLogicalOperators[Facts[Any], ResultSet[Any]](
        builder.andBuilder,
        builder.orBuilder,
        trueBuilder = alwaysMatch,
        falseBuilder = alwaysEmpty,
        input = JoeSchmoe.facts,
        trueOutput = FactsMatch(JoeSchmoe.facts),
        falseOutput = NoFactsMatch(),
      )
    }
  }

  "and / or" should {

    behave like validDslLogicalOperators {
      new DslLogicOpBuilder {
        import com.rallyhealth.vapors.core.dsl._

        override def andBuilder[T, A : Intersect]: LogicOpBuilder[T, A] = { (one, two, tail) =>
          and(one, two, tail: _*)
        }

        override def orBuilder[T, A : Union]: LogicOpBuilder[T, A] = { (one, two, tail) =>
          or(one, two, tail: _*)
        }
      }
    }
  }

  "&& / ||" should {

    behave like validDslLogicalOperators {
      new DslLogicOpBuilder {
        import com.rallyhealth.vapors.core.dsl._

        override def andBuilder[T, A : Intersect]: LogicOpBuilder[T, A] = { (one, two, tail) =>
          tail.foldLeft(one && two)(_ && _)
        }

        override def orBuilder[T, A : Union]: LogicOpBuilder[T, A] = { (one, two, tail) =>
          tail.foldLeft(one || two)(_ || _)
        }
      }
    }
  }
}
