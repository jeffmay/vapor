package com.rallyhealth.vapors

package core.math

/**
  * Defines multiplication for a specific type.
  */
trait Multiplication[A] {

  def multiply(
    lhs: A,
    rhs: A,
  ): A
}

object Multiplication extends NumericalImplicits {
  def apply[A](implicit A: Multiplication[A]): Multiplication[A] = A
}
