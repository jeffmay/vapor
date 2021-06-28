package com.rallyhealth

package vapors.interpreter

import vapors.dsl._
import vapors.example._

import cats.instances.order._
import org.scalatest.freespec.AnyFreeSpec
import shapeless.Nat

import scala.collection.View

class SelectFromOutputSpec extends AnyFreeSpec {

  "create a map from a converting a list of facts to tuples" in {
    val query = valuesOfType(FactTypes.TagsUpdate).map { update =>
      wrap(
        update.get(_.select(_.source)).returnOutput,
        update.get(_.select(_.tags)).returnOutput,
      ).asTuple.withOutputValue
    }.toMap
    val expected = User1.factTable
      .getSortedSeq(FactTypes.TagsUpdate)
      .map { fact =>
        (fact.value.source, fact.value.tags)
      }
      .toMap
    val result = eval(User1.factTable)(query)
    assertResult(expected) {
      result.output.value.toMap
    }
  }

  "create a set from a list using groupBy, flatMap, sorted, and headOption" in {
    val query = factsOfType(FactTypes.TagsUpdate)
      .groupBy(_.select(_.value.source))
      .flatMap { sourceAndFacts =>
        val facts = sourceAndFacts.getFoldable(_.at(Nat._1))
        val latestFactTags = facts.sorted.headOption.toSet.flatMap(_.getFoldable(_.select(_.value.tags)))
        latestFactTags.to(View)
      }
    val expected = User1.factTable.getSortedSeq(FactTypes.TagsUpdate).groupBy(_.value.source).view.flatMap {
      case (_, facts) =>
        facts.map(_.value).sorted.headOption.toList.flatMap(_.tags)
    }
    val result = eval(User1.factTable)(query)
    assertResult(expected.toVector) {
      result.output.value.toVector
    }
  }
}
