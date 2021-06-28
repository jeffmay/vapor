package com.rallyhealth

package vapors.example

import vapors.data.FactTable

import org.scalacheck.Gen
import org.scalacheck.rng.Seed

abstract class GeneratedUser {

  val name: String = getClass.getSimpleName.filterNot(Set('$'))

  val seed: Seed = Seed(name.##)

  lazy val factTable: FactTable =
    FactTable(Generators.genFact.pureApply(GeneratedUser.scalaCheckParams, seed))
}

object GeneratedUser {

  val scalaCheckParams: Gen.Parameters = Gen.Parameters.default
    .withInitialSeed(7)
    .withSize(7)
}
