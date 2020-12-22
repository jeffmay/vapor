package com.rallyhealth.vapors.core

package object math {

  type Numerical[A] = Addition[A] with Subtraction[A] with Multiplication[A] with Negative[A]
  type RealNumerical[A] = Numerical[A] with Division[A]
  type IntNumerical[A] = RealNumerical[A] with IntegralDivision[A]

}
