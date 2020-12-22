package com.rallyhealth.vapors.core.math

/**
  * Defines long division for an integral data type.
  *
  * This includes both the quotient (integer division) and remainder (modulo).
  */
trait IntegralDivision[A] extends Division[A] {

  override def divide(
    lhs: A,
    rhs: A,
  ): A = quotient(lhs, rhs)

  def quotient(
    lhs: A,
    rhs: A,
  ): A

  def remainder(
    lhs: A,
    rhs: A,
  ): A
}

object IntegralDivision extends NumericalImplicits {

  @inline final def apply[A](implicit A: IntegralDivision[A]): IntegralDivision[A] = A
}
