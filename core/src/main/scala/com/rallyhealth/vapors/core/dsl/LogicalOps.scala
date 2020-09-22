package com.rallyhealth.vapors.core.dsl

import com.rallyhealth.vapors.core.data.{Intersect, Union}

final class LogicalOps[T, A](private val exp: AnyExp[T, A]) extends AnyVal {

  def &&(o: AnyExp[T, A])(implicit A: Intersect[A]): AnyExp[T, A] = and(exp, o)

  def ||(o: AnyExp[T, A])(implicit A: Union[A]): AnyExp[T, A] = or(exp, o)
}
