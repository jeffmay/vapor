package com.rallyhealth.vapors

package core.interpreter

import core.data.{Evidence, FactTable}
import core.dsl._
import core.example.{FactTypes, JoeSchmoe}

import org.scalatest.Inside.inside
import org.scalatest.freespec.AnyFreeSpec

class ConcatOutputSpec extends AnyFreeSpec {

  "concat should" - {

    "combine all elements into the given traversable type" in {
      val factTypes = List(FactTypes.TagsUpdate, FactTypes.WeightMeasurement, FactTypes.WeightSelfReported)
      val expectedFacts = factTypes.flatMap(JoeSchmoe.factTable.getSortedSeq(_))
      val instantQueries = factTypes.map { factType =>
        factsOfType(factType).map(_.value.get(_.select(_.timestamp))).returnOutput
      }
      val query = concat(instantQueries: _*).toOutputMonoid
      val result = eval(JoeSchmoe.factTable)(query)
      assertResult(expectedFacts.map(_.value.timestamp))(result.output.value)
      assertResult(Evidence(expectedFacts)) {
        result.output.evidence
      }
    }

    "combine an empty list of expressions into an empty list" in {
      val query = concat[FactTable, List, Nothing, Unit]().toOutputMonoid
      val result = eval(JoeSchmoe.factTable)(query)
      assertResult(Nil)(result.output.value)
      assertResult(Evidence.none) {
        result.output.evidence
      }
    }

    "concat mixed elements types into a common supertype" in {
      val factTypes = List(FactTypes.TagsUpdate, FactTypes.WeightMeasurement, FactTypes.WeightSelfReported)
      val expectedFacts = factTypes.flatMap(JoeSchmoe.factTable.getSortedSeq(_))
      val instantQueries = factTypes.map(factsOfType(_).returnOutput)
      val query = concat(instantQueries: _*).toOutputMonoid.withOutputFoldable.map(_.value.get(_.select(_.timestamp)))
      val result = eval(JoeSchmoe.factTable)(query)
      assertResult(expectedFacts.map(_.value.timestamp))(result.output.value)
      assertResult(Evidence(expectedFacts)) {
        result.output.evidence
      }
    }

    "combine all elements into a LazyList" in {
      val factTypes = List(FactTypes.TagsUpdate, FactTypes.WeightMeasurement, FactTypes.WeightSelfReported)
      val expectedFacts = factTypes.flatMap(JoeSchmoe.factTable.getSortedSeq(_))
      val instantQueries = factTypes.map { factType =>
        factsOfType(factType).map(_.value.get(_.select(_.timestamp))).returnOutput
      }
      val query = concat(instantQueries: _*).toLazyList
      val result = eval(JoeSchmoe.factTable)(query)
      assertResult(expectedFacts.map(_.value.timestamp))(result.output.value)
      assertResult(Evidence(expectedFacts)) {
        result.output.evidence
      }
    }

    "combined all elements into a LazyList without forcing the values" in {
      val factTypes = List(FactTypes.TagsUpdate, FactTypes.WeightMeasurement, FactTypes.WeightSelfReported)
      val instantQueries = factTypes.map { factType =>
        factsOfType(factType).map(_.value.get(_.select(_.timestamp))).returnOutput
      }
      val query = concat(instantQueries: _*).toLazyList
      val result = eval(JoeSchmoe.factTable)(query)
      inside(result.output.value) {
        case values: LazyList[_] =>
          // confirm that the collection does not start with a definite size because it was unforced
          assert(!values.hasDefiniteSize)
          // confirm that forcing the collection causes it to have a definite size
          assert(values.force.hasDefiniteSize)
      }
    }

    "combine an empty list of expressions into an empty lazy list" in {
      val query = concat[FactTable, List, Nothing, Unit]().toLazyList
      val result = eval(JoeSchmoe.factTable)(query)
      assertResult(LazyList.empty)(result.output.value)
      assertResult(Evidence.none) {
        result.output.evidence
      }
    }
  }
}
