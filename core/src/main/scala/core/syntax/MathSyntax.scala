package com.rallyhealth.vapors

package core.syntax

import core.math._

trait MathSyntax {

  implicit def math[A](value: A): MathOps[A] = new MathOps(value)
}

final class MathOps[A](private val lhs: A) extends AnyVal {

  def +(rhs: A)(implicit A: Addition[A]): A = A.add(lhs, rhs)

  def -(rhs: A)(implicit A: Subtraction[A]): A = A.subtract(lhs, rhs)

  def unary_-(implicit A: Negative[A]): A = A.negative(lhs)

  def *(rhs: A)(implicit A: Multiplication[A]): A = A.multiply(lhs, rhs)

  def /(rhs: A)(implicit A: Division[A]): A = A.quot(lhs, rhs)
}
