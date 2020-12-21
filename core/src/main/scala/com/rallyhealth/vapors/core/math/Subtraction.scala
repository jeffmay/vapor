package com.rallyhealth.vapors.core.math

/**
  * Defines subtraction of two values for a specific type.
  *
  * @note order of arguments matter, as subtraction is not commutative.
  */
trait Subtraction[A] {

  def subtract(
    lhs: A,
    rhs: A,
  ): A
}

object Subtraction extends NumericalImplicits {

  @inline final def apply[A](implicit A: Subtraction[A]): Subtraction[A] = A
}
