package com.rallyhealth.vapors

package core.math

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

  def apply[A](implicit A: Addition[A]): Addition[A] = A
}
