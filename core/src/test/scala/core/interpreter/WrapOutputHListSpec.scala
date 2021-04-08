package com.rallyhealth.vapors

package core.interpreter

import core.data.{Evidence, FactTable}
import core.dsl._
import core.example.{ColorCoding, FactTypes, JoeSchmoe, TagsUpdate}

import org.scalatest.freespec.AnyFreeSpec
import shapeless.HNil

class WrapOutputHListSpec extends AnyFreeSpec {

  import core.example.SimpleTagUpdates._

  "wrap, when given const nodes, should" - {

    "convert to a case class with the correct number of inputs" in {
      val query = {
        wrap(const(tagsNow.source), const(tagsNow.tags), const(tagsNow.timestamp)).as[TagsUpdate]
      }
      val result = eval(FactTable.empty)(query)
      assertResult(tagsNow)(result.output.value)
    }

    "NOT compile when attempting to convert fewer inputs than required into a case class" in {
      assertDoesNotCompile {
        "wrap(const(tagsNow.tags)).as[TagsUpdate]"
      }
    }

    "NOT compile when attempting to convert inputs into a case class in the wrong order" in {
      assertDoesNotCompile {
        "wrap(const(tagsNow.tags), const(tagsNow.timestamp), const(tagsNow.source)).as[TagsUpdate]"
      }
    }

    "convert into a tuple-3" in {
      val t = (1, "two", ColorCoding.Blue)
      val query = {
        wrap(const(t._1), const(t._2), const(t._3)).asTuple
      }
      val result = eval(FactTable.empty)(query)
      assertResult(t)(result.output.value)
    }

    "convert into an hlist of size 3" in {
      val hlist = 1 :: "two" :: ColorCoding.Blue :: HNil
      val t = hlist.tupled
      val query = {
        wrap(const(t._1), const(t._2), const(t._3)).asHList
      }
      val result = eval(FactTable.empty)(query)
      assertResult(hlist)(result.output.value)
    }
  }

  "wrap, when comparing evidence, should" - {

    "combine all non-empty evidence" in {
      val query = {
        wrap(
          factsOfType(FactTypes.Name).headOption,
          factsOfType(FactTypes.Age).headOption,
        ).asHList
      }
      val facts = List(JoeSchmoe.name, JoeSchmoe.age)
      val result = eval(FactTable(facts))(query)
      assertResult(Evidence(facts))(result.output.evidence)
    }

    "return no evidence if any branch has no evidence" in {
      val query = {
        wrap(
          factsOfType(FactTypes.Name).headOption,
          factsOfType(FactTypes.Age).headOption,
        ).asHList
      }
      val result = eval(FactTable(JoeSchmoe.name))(query)
      assertResult(Evidence.none)(result.output.evidence)
    }
  }

}
