package com.rallyhealth.vapors

package core.interpreter

import core.data.{Evidence, FactTable, FactType}
import core.dsl._
import core.example.{FactTypes, HasTimestamp, JoeSchmoe}

import org.scalatest.Inside.inside
import org.scalatest.freespec.AnyFreeSpec

class WrapOutputSeqSpec extends AnyFreeSpec {

  "wrapSeq / sequence should" - {

    "wrap a list of constant expressions into an expression of a list of the values" in {
      val query = wrapSeq(
        const(1),
        const(2),
        const(3),
      )
      val result = eval(FactTable.empty)(query)
      assertResult(Seq(1, 2, 3)) {
        result.output.value
      }
    }

    "not force the resulting lazy list" in {
      val query = wrapSeq(
        const(1),
        const(2),
        const(3),
      )
      val result = eval(FactTable.empty)(query)
      inside(result.output.value) {
        case values: LazyList[_] =>
          // confirm that the collection does not start with a definite size because it was unforced
          assert(!values.hasDefiniteSize)
          // confirm that forcing the collection causes it to have a definite size
          assert(values.force.hasDefiniteSize)
      }
    }

    "wrap a list of expressions and combine the evidence from all of them" in {
      val factTypes = List[FactType[_ <: HasTimestamp]](
        FactTypes.BloodPressureMeasurement,
        FactTypes.WeightSelfReported,
        FactTypes.TagsUpdate,
      )
      val subExpressions =
        factTypes.map(t => factsOfType(t).map(_.value.get(_.select(_.timestamp))).returnOutput)
      val query = sequence(subExpressions)
      val factsPerType = factTypes.map(JoeSchmoe.factTable.getSortedSeq(_))
      val expectedValues = factsPerType.map(_.map(_.value.timestamp))
      val result = eval(JoeSchmoe.factTable)(query)
      assertResult(expectedValues) {
        result.output.value
      }
      assertResult(Evidence(factsPerType.flatten)) {
        result.output.evidence
      }
    }
  }
}
