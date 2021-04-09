package com.rallyhealth

package vapors.data

/**
  * Extracts a value from some starting value.
  *
  * This is useful for defining [[CaptureP]] based on some input
  * type, without having to share a common supertype. Although, because of variance rules, this makes it
  * easy to define an [[ExtractValue]] for a supertype, if desired.
  */
trait ExtractValue[-T, +V] {
  def extractValue(obj: T): V
}

object ExtractValue {
  @inline final def apply[V]: Of[V] = new Of[V]
  final class Of[V] private[ExtractValue] (private val dummy: Boolean = true) extends AnyVal {

    def apply[T](input: T)(implicit extractor: ExtractValue[T, V]): V = extractor.extractValue(input)

    def from[T](implicit extractor: ExtractValue[T, V]): ExtractValue[T, V] = extractor
  }

  implicit def id[T]: ExtractValue[T, T] = identity[T]

  implicit class ExtractBooleanOps[T](private val extractBoolean: ExtractBoolean[T]) extends AnyVal {

    @deprecated("Use ExtractValue[Boolean](value) or .extractValue(value) instead.", "0.14.1")
    def isTrue(value: T): Boolean = extractBoolean.extractValue(value)
  }
}
