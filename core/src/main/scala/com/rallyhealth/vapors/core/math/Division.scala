package com.rallyhealth.vapors.core.math

/**
  * Defines division of two values for a specific type.
  */
trait Division[A] {

  def divide(
    lhs: A,
    rhs: A,
  ): A
}

object Division extends NumericalImplicits {

  @inline final def apply[A](implicit A: Division[A]): Division[A] = A
}
