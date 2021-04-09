package com.rallyhealth

package vapors.interpreter

import vapors.dsl._
import vapors.example.{FactTypes, User1}

import org.scalatest.freespec.AnyFreeSpec

class GroupOutputSpec extends AnyFreeSpec {

  "groupBy should" - {

    "create a map from a list using groupBy and mapValues" in {
      val query = factsOfType(FactTypes.TagsUpdate)
        .groupBy(_.select(_.value.source))
        // TODO: This is very verbose and doesn't easily support the common case of mapping over collection values
        .mapValues(_.returnOutput.withOutputFoldable.map(_.value).returnOutput)
      val expected = User1.factTable.getSortedSeq(FactTypes.TagsUpdate).groupMap(_.value.source)(_.value)
      val result = eval(User1.factTable)(query)
      assertResult(expected) {
        result.output.value.toMap
      }
    }
  }
}
