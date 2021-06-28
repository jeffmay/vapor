package com.rallyhealth

package vapors.math

/**
  * Defines division for a specific type.
  *
  * @note order of arguments matter, as division is not commutative.
  */
trait Division[A] {

  def quot(
    dividend: A,
    divisor: A,
  ): A // quotient = dividend/divisor
}

object Division extends NumericalImplicits {
  def apply[A](implicit A: Division[A]): Division[A] = A
}
