package com.rallyhealth.vapors.core.dsl

import com.rallyhealth.vapors.core.data.{Intersect, Union}
import com.rallyhealth.vapors.core.dsl

final class LogicalOps[T, A](private val exp: AnyExp[T, A]) extends AnyVal {

  /**
    * Builds an AND expression using infix notation.
    */
  def and(o: AnyExp[T, A])(implicit A: Intersect[A]): AnyExp[T, A] = dsl.and(exp, o)

  /**
    * Same as [[and]] but with higher precedent.
    */
  def &&(o: AnyExp[T, A])(implicit A: Intersect[A]): AnyExp[T, A] = dsl.and(exp, o)

  /**
    * Builds an OR expression using infix notation.
    */
  def or(o: AnyExp[T, A])(implicit A: Union[A]): AnyExp[T, A] = dsl.or(exp, o)

  /**
    * Same as [[or]] but with higher precedent.
    */
  def ||(o: AnyExp[T, A])(implicit A: Union[A]): AnyExp[T, A] = dsl.or(exp, o)
}
