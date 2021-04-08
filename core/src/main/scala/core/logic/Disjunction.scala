package com.rallyhealth.vapors

package core.logic

import core.interpreter.InterpretExprAsResultFn

import cats.{Invariant, Semigroupal}

/**
  * Defines logical disjunction (aka OR) for a specific type.
  *
  * @see for more details on how this works, check out
  *      [[InterpretExprAsResultFn.Output.disjunction]]
  */
trait Disjunction[A] {

  def disjunction(
    lhs: A,
    rhs: A,
  ): A
}

object Disjunction {

  @inline final def apply[A](implicit A: Disjunction[A]): Disjunction[A] = A

  implicit final object BooleanDisjunction extends Disjunction[Boolean] {
    override def disjunction(
      lhs: Boolean,
      rhs: Boolean,
    ): Boolean = lhs || rhs
  }

  implicit final object FunctorInstances extends Invariant[Disjunction] with Semigroupal[Disjunction] {

    override def product[A, B](
      fa: Disjunction[A],
      fb: Disjunction[B],
    ): Disjunction[(A, B)] = { (lhs: (A, B), rhs: (A, B)) =>
      (fa.disjunction(lhs._1, lhs._1), fb.disjunction(lhs._2, rhs._2))
    }

    override def imap[A, B](fa: Disjunction[A])(f: A => B)(g: B => A): Disjunction[B] = { (lhs: B, rhs: B) =>
      f(fa.disjunction(g(lhs), g(rhs)))
    }
  }
}
