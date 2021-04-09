package com.rallyhealth

package vapors.math

/**
  * Defines inverting a single value to it's negative.
  *
  * @note not to be confused with [[com.rallyhealth.vapors.core.logic.Negation]], which is the formal logic
  *       definition of negation of the conclusion.
  */
trait Negative[A] {

  def negative(value: A): A
}

object Negative extends NumericalImplicits {

  def apply[A](implicit A: Negative[A]): Negative[A] = A
}
