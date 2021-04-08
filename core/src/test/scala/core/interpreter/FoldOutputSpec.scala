package com.rallyhealth.vapors

package core.interpreter

import core.data.FactTable
import core.dsl._

import org.scalatest.freespec.AnyFreeSpec

class FoldOutputSpec extends AnyFreeSpec {

  "Expr.FoldOutput" - {

    "fold a list of ints into its sum" in {
      val query = const(List(1, 2, 3)).withOutputFoldable.fold
      val result = eval(FactTable.empty)(query)
      assertResult(6)(result.output.value)
    }

    "fold a list of options of int into an option containing the sum" in {
      val query = const(List(Some(1), None, Some(3))).withOutputFoldable.fold
      val result = eval(FactTable.empty)(query)
      assertResult(Some(4))(result.output.value)
    }

    "fold a list of options of int into a None" in {
      val query = const(List[Option[Int]](None, None, None)).withOutputFoldable.fold
      val result = eval(FactTable.empty)(query)
      assertResult(None)(result.output.value)
    }
  }
}
