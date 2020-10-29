package com.rallyhealth.vapors.core.syntax

import com.rallyhealth.vapors.core.math.{Addition, Negative, Subtraction}

trait MathSyntax {

  implicit def math[A](value: A): MathOps[A] = new MathOps(value)
}

final class MathOps[A](private val lhs: A) extends AnyVal {

  def +(rhs: A)(implicit A: Addition[A]): A = A.add(lhs, rhs)

  def -(rhs: A)(implicit A: Subtraction[A]): A = A.subtract(lhs, rhs)

  def unary_-(implicit A: Negative[A]): A = A.negative(lhs)
}
