package com.rallyhealth.vapors.core.evaluator

import com.rallyhealth.vapors.core.logic.{Intersect, Union}
import com.rallyhealth.vapors.factfilter.Example.JoeSchmoe
import com.rallyhealth.vapors.factfilter.data.{Facts, FactsMatch, NoFactsMatch, ResultSet, TypedFactsMatch}
import com.rallyhealth.vapors.factfilter.dsl._
import org.scalatest.wordspec.AnyWordSpec

import scala.language.existentials

// TODO: Split out generic logical tests so that other types can be tested with these logical operations
final class LogicalOperatorSpec extends AnyWordSpec {

  type LogicOpBuilder[T, A] = (Exp[T, A], Exp[T, A], Seq[Exp[T, A]]) => Exp[T, A]

  private def validLogicalOperators[T, A : Intersect : Union](
    andBuilder: LogicOpBuilder[T, A],
    orBuilder: LogicOpBuilder[T, A],
    trueBuilder: Exp[T, A],
    falseBuilder: Exp[T, A],
    input: T,
    trueOutput: A,
    falseOutput: A,
  ): Unit = {

    val T = trueBuilder
    val F = falseBuilder

    def and(
      one: Exp[T, A],
      two: Exp[T, A],
      tail: Exp[T, A]*,
    ): Exp[T, A] = andBuilder(one, two, tail)

    def or(
      one: Exp[T, A],
      two: Exp[T, A],
      tail: Exp[T, A]*,
    ): Exp[T, A] = orBuilder(one, two, tail)

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

      behave like validLogicalOperators[Facts, ResultSet](
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
