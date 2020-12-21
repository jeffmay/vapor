package com.rallyhealth.vapors.core.math

/**
  * Defines addition of two values for a specific type.
  */
trait Addition[A] {

  def add(
    lhs: A,
    rhs: A,
  ): A
}

object Addition extends NumericalImplicits {

  @inline final def apply[A](implicit A: Addition[A]): Addition[A] = A
}
