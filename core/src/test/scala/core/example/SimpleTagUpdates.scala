package com.rallyhealth.vapors

package core.example

import java.time.Instant

object SimpleTagUpdates {

  private val now = Instant.now()
  final val tagsNow = TagsUpdate("xSource", Set("X1", "X2"), now)
  final val tags5MinAgo = TagsUpdate("ySource", Set("Y1"), now.minusSeconds(5 * 60))
  final val tags15MinAgo = TagsUpdate("zSource", Set("Z1"), now.minusSeconds(15 * 60))
}
