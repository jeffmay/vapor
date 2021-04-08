package com.rallyhealth.vapors

package core.logic

import core.interpreter.InterpretExprAsResultFn

import cats.{Invariant, Semigroupal}

/**
  * Defines logical conjunction (aka AND) for a specific type.
  *
  * @see for more details on how this works, check out
  *      [[InterpretExprAsResultFn.Output.conjunction]]
  */
trait Conjunction[A] {

  def conjunction(
    lhs: A,
    rhs: A,
  ): A
}

object Conjunction {

  @inline final def apply[A](implicit A: Conjunction[A]): Conjunction[A] = A

  implicit final object BooleanConjunction extends Conjunction[Boolean] {
    override def conjunction(
      lhs: Boolean,
      rhs: Boolean,
    ): Boolean = lhs && rhs
  }

  implicit final object FunctorInstances extends Invariant[Conjunction] with Semigroupal[Conjunction] {

    override def product[A, B](
      fa: Conjunction[A],
      fb: Conjunction[B],
    ): Conjunction[(A, B)] = { (lhs: (A, B), rhs: (A, B)) =>
      (fa.conjunction(lhs._1, rhs._1), fb.conjunction(lhs._2, rhs._2))
    }

    override def imap[A, B](fa: Conjunction[A])(f: A => B)(g: B => A): Conjunction[B] = { (lhs: B, rhs: B) =>
      f(fa.conjunction(g(lhs), g(rhs)))
    }
  }
}
