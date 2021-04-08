package com.rallyhealth.vapors

package core.example

import core.data.Fact

import org.scalacheck.Gen
import org.scalacheck.ops._

import java.time.Instant

object Generators {

  lazy val genSmallId: Gen[String] = for {
    chars <- Gen.stringOfN(1, Gen.alphaUpperChar)
    nums <- Gen.stringOfN(2, Gen.numChar)
  } yield chars + nums

  lazy val genMusicalNote: Gen[Char] = Gen.choose('A', 'G')

  lazy val genMusicalNoteOrEmpty: Gen[String] = Gen.option(Gen.stringOfN(1, genMusicalNote)).map(_.getOrElse(""))

  lazy val genTimestampWithin90Days: Gen[Instant] = Gen.javaInstant.beforeNowWithin(java.time.Duration.ofDays(90))

  def genTagsSource: Gen[String] = genMusicalNoteOrEmpty

  lazy val genTags: Gen[Set[String]] = Gen.setOf(genSmallId)

  lazy val genTagsUpdate: Gen[TagsUpdate] = for {
    source <- genTagsSource
    tags <- genTags
    timestamp <- genTimestampWithin90Days
  } yield TagsUpdate(
    source,
    tags,
    timestamp,
  )

  lazy val genFact: Gen[Fact] = genTagsUpdate.map(FactTypes.TagsUpdate)
}
