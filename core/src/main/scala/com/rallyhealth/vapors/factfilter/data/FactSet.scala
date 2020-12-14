package com.rallyhealth.vapors.factfilter.data

import scala.collection.immutable.SortedSet

object FactSet {

  def apply(facts: Fact*): FactSet = SortedSet(facts: _*)
}
