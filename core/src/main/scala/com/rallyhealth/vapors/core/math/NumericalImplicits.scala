package com.rallyhealth.vapors.core.math

/**
  * Defines default implementations for [[Numeric]]-based arithmetic operators.
  */
private[math] trait NumericalImplicits {

  implicit def numeric[A : Numeric]: FromNumeric[A] = new FromNumeric[A]
}

/**
  * Defines all arithmetic type-classes from Scala's [[Numeric]] definition.
  */
final class FromNumeric[A : Numeric] extends Addition[A] with Subtraction[A] with Negative[A] {
  import Numeric.Implicits._

  override def add(
    lhs: A,
    rhs: A,
  ): A = lhs + rhs

  override def subtract(
    lhs: A,
    rhs: A,
  ): A = lhs - rhs

  override def negative(value: A): A = -value
}
