package com.rallyhealth.vapors.factfilter.evaluator

import cats.Id
import com.rallyhealth.vapors.core.algebra.Expr
import com.rallyhealth.vapors.core.logic._
import com.rallyhealth.vapors.factfilter.Example.JoeSchmoe
import com.rallyhealth.vapors.factfilter.data.{Evidence, ExtractBoolean, FactTable, Facts}
import com.rallyhealth.vapors.factfilter.dsl.ExprDsl._
import com.rallyhealth.vapors.factfilter.dsl.{CaptureP, ExprDsl}
import com.rallyhealth.vapors.factfilter.evaluator.InterpretExprAsFunction.{Input, Output}
import org.scalactic.source.Position
import org.scalatest.wordspec.AnyWordSpec

class LogicalExprSpec extends AnyWordSpec {

  private type LogicExpr[R] = Expr[Id, Unit, R, Unit]

  private type LogicOpBuilder[R] =
    (LogicExpr[R], LogicExpr[R], Seq[LogicExpr[R]]) => LogicExpr[R]

  private type UnaryLogicOpBuilder[R] = LogicExpr[R] => LogicExpr[R]

  private def evalUnit[R](facts: Facts)(expr: LogicExpr[R]): Output[R] = {
    InterpretExprAsFunction(expr)(Input[Id, Unit]((), Evidence(facts.toList), FactTable(facts.toList))).output
  }

  private def validLogicalOperators[R](
    andBuilder: LogicOpBuilder[R],
    orBuilder: LogicOpBuilder[R],
    notBuilder: UnaryLogicOpBuilder[R],
    trueBuilder: LogicExpr[R],
    falseBuilder: LogicExpr[R],
    facts: Facts,
    assertTrue: Position => Output[R] => Unit,
    assertFalse: Position => Output[R] => Unit,
  ): Unit = {

    val T = trueBuilder
    val F = falseBuilder
    val not = notBuilder

    def and(
      one: LogicExpr[R],
      two: LogicExpr[R],
      tail: LogicExpr[R]*,
    ): LogicExpr[R] = andBuilder(one, two, tail)

    def or(
      one: LogicExpr[R],
      two: LogicExpr[R],
      tail: LogicExpr[R]*,
    ): LogicExpr[R] = orBuilder(one, two, tail)

    val evalOutput = evalUnit[R](facts)(_)

    def shouldBeTrue(output: Output[R])(implicit pos: Position): Unit = {
      assertTrue(pos)(output)
    }

    def shouldBeFalse(output: Output[R])(implicit pos: Position): Unit = {
      assertFalse(pos)(output)
    }

    "return 'false' for 'not true'" in {
      val q = {
        not(T)
      }
      shouldBeFalse(evalOutput(q))
    }

    "return 'true' for 'not false'" in {
      val q = {
        not(F)
      }
      shouldBeTrue(evalOutput(q))
    }

    "return 'true' for double negation of 'true'" in {
      val q = {
        not(not(T))
      }
      shouldBeTrue(evalOutput(q))
    }

    "return 'false' for double negation of 'false'" in {
      val q = {
        not(not(F))
      }
      shouldBeFalse(evalOutput(q))
    }

    "return 'true' for one 'true' and one 'false' in an 'or'" in {
      val q = {
        or(T, F)
      }
      shouldBeTrue(evalOutput(q))
    }

    "return 'true' for one 'false' and one 'not false' in an 'or'" in {
      val q = {
        or(F, not(F))
      }
      shouldBeTrue(evalOutput(q))
    }

    "return 'false' for one 'false' and one 'not true' in an 'or'" in {
      val q = {
        or(F, not(T))
      }
      shouldBeFalse(evalOutput(q))
    }

    "return 'true' for a complex 'or'" in {
      val q = {
        or(F, F, T, F)
      }
      shouldBeTrue(evalOutput(q))
    }

    "return 'false' for negating a complex 'or'" in {
      val q = {
        not(or(F, not(T), T, not(not(F))))
      }
      shouldBeFalse(evalOutput(q))
    }

    "return 'false' for a long 'or'" in {
      val q = {
        or(F, F, F, F)
      }
      shouldBeFalse(evalOutput(q))
    }

    "return 'true' for negating a long 'or'" in {
      val q = {
        not(or(F, F, not(T), not(not(F))))
      }
      shouldBeTrue(evalOutput(q))
    }

    "return 'false' for a 'true' and 'false' in an 'and'" in {
      val q = {
        and(T, F)
      }
      shouldBeFalse(evalOutput(q))
    }

    "return 'false' for a 'true' and 'not true' in an 'and'" in {
      val q = {
        and(T, not(T))
      }
      shouldBeFalse(evalOutput(q))
    }

    "return 'true' for two trues in an and" in {
      val q = {
        and(T, T)
      }
      shouldBeTrue(evalOutput(q))
    }

    "return 'true' for two 'not false's in an 'and'" in {
      val q = {
        and(not(F), not(F))
      }
      shouldBeTrue(evalOutput(q))
    }

    "return 'false' for a complex 'and'" in {
      val q = {
        and(T, T, F, T)
      }
      shouldBeFalse(evalOutput(q))
    }

    "return 'true' for the negation of a complex 'and'" in {
      val q = {
        not(and(T, not(F), F, not(not(T))))
      }
      shouldBeTrue(evalOutput(q))
    }

    "return 'true' for a long 'and'" in {
      val q = {
        and(T, T, T, T)
      }
      shouldBeTrue(evalOutput(q))
    }

    "return 'false' for the negation of a long 'and'" in {
      val q = {
        not(and(T, not(F), not(not(T)), not(not(not(F)))))
      }
      shouldBeFalse(evalOutput(q))
    }

    "return true for nested true 'or's in an 'and'" in {
      val q = {
        and(or(T, T), or(T, T))
      }
      shouldBeTrue(evalOutput(q))
    }

    "return 'true' for two nested 'not false or's in an 'and'" in {
      val q = {
        and(not(or(F, F)), not(or(F, F)))
      }
      shouldBeTrue(evalOutput(q))
    }

    "return 'false' for a nested 'false or' before some 'true' expressions" in {
      val q = {
        and(or(F, F), T, T)
      }
      shouldBeFalse(evalOutput(q))
    }

    "return 'true' for a nested 'not true or' before some 'true' expressions" in {
      val q = {
        and(not(or(F, T)), T, not(F))
      }
      shouldBeFalse(evalOutput(q))
    }

    "return 'false' for a nested 'false or' after some 'true' expressions" in {
      val q = {
        and(T, or(T, F), or(F, F))
      }
      shouldBeFalse(evalOutput(q))
    }

    "return 'false' for a nested 'not true or' after some 'true' expressions" in {
      val q = {
        and(T, not(F), or(not(T), T), or(F, not(T)))
      }
      shouldBeFalse(evalOutput(q))
    }

    "return 'true' for a nested 'false and' in an 'or'" in {
      val q = {
        or(F, and(F, F), and(T, T), and(T, F))
      }
      shouldBeTrue(evalOutput(q))
    }

    "return 'false' for the negation of a nested 'false and' in an 'or'" in {
      val q = {
        not(or(F, and(F, F), not(and(T, T)), and(T, not(F), not(T))))
      }
      shouldBeTrue(evalOutput(q))
    }

    "return 'true' for a complex structure" in {
      val q = {
        or(F, and(F, T), or(F, F), and(or(T, F), F), or(and(T, F), T))
      }
      shouldBeTrue(evalOutput(q))
    }

    "return 'false' for a complex structure" in {
      val q = {
        not(
          or(
            F,
            not(T),
            and(F, T),
            or(F, F),
            not(and(T, T)),
            not(or(T, F)),
            not(not(F)),
            and(or(T, F), F, not(T)),
            or(F, not(T), and(T, F), not(and(T, not(F)))),
          ),
        )
      }
      shouldBeTrue(evalOutput(q))
    }
  }

  private abstract class DslLogicOpBuilder {

    def andBuilder[R : Conjunction : ExtractBoolean]: LogicOpBuilder[R]

    def orBuilder[R : Disjunction : ExtractBoolean]: LogicOpBuilder[R]

    def notBuilder[R : Negation]: UnaryLogicOpBuilder[R]
  }

  def validDslLogicalOperators(builder: DslLogicOpBuilder): Unit = {

    "operating on boolean results" should {

      behave like validLogicalOperators[Boolean](
        builder.andBuilder,
        builder.orBuilder,
        builder.notBuilder,
        trueBuilder = const(true),
        falseBuilder = const(false),
        facts = JoeSchmoe.facts,
        assertTrue = { implicit pos => o =>
          assert(o.value)
          // TODO: Is is sufficient to only test all the facts or no facts?
          assertResult(Evidence(JoeSchmoe.facts.toList))(o.evidence)
        },
        assertFalse = { implicit pos => o =>
          assert(!o.value)
          assertResult(Evidence(JoeSchmoe.facts.toList))(o.evidence)
        },
      )
    }
  }

  "and / or" should {

    behave like validDslLogicalOperators {
      new DslLogicOpBuilder {

        override def andBuilder[R : Conjunction : ExtractBoolean]: LogicOpBuilder[R] = { (one, two, tail) =>
          ExprDsl.and(one, two, tail: _*)
        }

        override def orBuilder[R : Disjunction : ExtractBoolean]: LogicOpBuilder[R] = { (one, two, tail) =>
          ExprDsl.or(one, two, tail: _*)
        }

        override def notBuilder[R : Negation]: UnaryLogicOpBuilder[R] = {
          ExprDsl.not(_)
        }
      }
    }
  }
}
