package com.rallyhealth.vapors.core.math

/**
  * Defines default implementations for [[Numeric]]-based arithmetic operators.
  */
private[math] trait NumericalImplicits {

  implicit def numeric[A : Numeric]: FromNumeric[A] = new FromNumeric[A]

  implicit def fractional[A : Fractional]: Division[A] = new FromFractional[A]

  implicit def integral[A : Integral]: IntegralDivision[A] = new FromIntegral[A]
}

/**
  * Defines all arithmetic type-classes from Scala's [[Numeric]] definition.
  */
class FromNumeric[A : Numeric] extends Addition[A] with Subtraction[A] with Negative[A] with Multiplication[A] {
  import Numeric.Implicits._

  override def add(
    lhs: A,
    rhs: A,
  ): A = lhs + rhs

  override def subtract(
    lhs: A,
    rhs: A,
  ): A = lhs - rhs

  override def multiply(
    lhs: A,
    rhs: A,
  ): A = lhs * rhs

  override def negative(value: A): A = -value
}

class FromFractional[A : Fractional] extends FromNumeric[A] with Division[A] {
  import Fractional.Implicits._

  override def divide(
    lhs: A,
    rhs: A,
  ): A = lhs / rhs
}

class FromIntegral[A : Integral] extends FromNumeric[A] with IntegralDivision[A] {
  import Integral.Implicits._

  override def quotient(
    lhs: A,
    rhs: A,
  ): A = lhs / rhs

  override def remainder(
    lhs: A,
    rhs: A,
  ): A = lhs % rhs
}
