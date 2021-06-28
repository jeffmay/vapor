package com.rallyhealth

package vapors.syntax

import vapors.lens.Indexed

import cats.Semigroup
import cats.data.NonEmptySet

trait IndexedSyntax {

  implicit def indexedOps[C](collection: C): IndexedOps[C] = new IndexedOps(collection)
}

final class IndexedOps[C](private val collection: C) extends AnyVal {

  def get[K, V](key: K)(implicit indexed: Indexed[C, K, V]): V = indexed.get(collection)(key)

  def filterKeys[K, V](
    keys: NonEmptySet[K],
  )(implicit
    indexed: Indexed[C, K, V],
    semigroupM: Semigroup[V],
  ): V = keys.toNonEmptyList.map(get(_)).reduce
}
