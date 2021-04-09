package com.rallyhealth

package vapors.logic

import cats.{Invariant, Semigroupal}

/**
  * Defines logical negaction (aka NOT) for a specific type.
  *
  * @see for more details on how this works, check out
  *      [[InterpretExprAsResultFn.Output.negation]]
  */
trait Negation[A] {
  def negation(value: A): A
}

object Negation {

  @inline final def apply[A](implicit A: Negation[A]): Negation[A] = A

  implicit final object BooleanNegation extends Negation[Boolean] {
    override def negation(value: Boolean): Boolean = !value
  }

  implicit final object InvariantInstances extends Invariant[Negation] with Semigroupal[Negation] {

    override def product[A, B](
      fa: Negation[A],
      fb: Negation[B],
    ): Negation[(A, B)] = { value =>
      (fa.negation(value._1), fb.negation(value._2))
    }

    override def imap[A, B](fa: Negation[A])(f: A => B)(g: B => A): Negation[B] = { value =>
      f(fa.negation(g(value)))
    }
  }
}
