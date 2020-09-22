package com.rallyhealth.vapors.core

package object evaluator {
  import cats.instances.function._
  import dsl._

  def eval[T, A](input: T)(exp: AnyExp[T, A]): A = {
    exp.foldMap(new EvalLoop[T]).apply(input)
  }

}
