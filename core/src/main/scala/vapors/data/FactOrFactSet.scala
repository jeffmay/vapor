package com.rallyhealth

package vapors.data

/**
  * A magnet type for passing individual [[Fact]]s and collections of [[Fact]]s into the same variable argument list.
  */
final class FactOrFactSet private[FactOrFactSet] (val toSet: Set[Fact]) extends AnyVal

object FactOrFactSet {

  implicit def setOfOneFact(fact: Fact): FactOrFactSet = new FactOrFactSet(Set(fact))

  implicit def iterableSetOfFacts(facts: Iterable[Fact]): FactOrFactSet =
    new FactOrFactSet(Set.from(facts))

  def flatten(factOrFactSets: Iterable[FactOrFactSet]): FactSet = {
    factOrFactSets.foldLeft(FactSet.empty)(_ | _.toSet)
  }
}
