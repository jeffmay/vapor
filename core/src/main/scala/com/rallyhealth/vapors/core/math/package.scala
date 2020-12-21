package com.rallyhealth.vapors.core

package object math {

  type Linear[A] = Addition[A] with Subtraction[A] with Multiplication[A]
  type Numerical[A] = Linear[A] with Negative[A]

}
