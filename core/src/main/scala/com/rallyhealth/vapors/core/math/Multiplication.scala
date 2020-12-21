package com.rallyhealth.vapors.core.math

/**
  * Defines multiplication of two values for a specific type.
  */
trait Multiplication[A] {

  def multiply(
    lhs: A,
    rhs: A,
  ): A
}

object Multiplication extends NumericalImplicits {

  @inline final def apply[A](implicit A: Multiplication[A]): Multiplication[A] = A
}
