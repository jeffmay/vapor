package com.rallyhealth.vapors.factfilter.dsl

import cats.Eq
import cats.free.FreeApplicative
import com.rallyhealth.vapors.core.algebra.ExpAlg
import com.rallyhealth.vapors.core.data.Window
import com.rallyhealth.vapors.core.logic.{Intersect, Union}
import com.rallyhealth.vapors.factfilter.data._

private[dsl] class Dsl extends TypedFactOps {

  def forall[T, V](cond: CondExp[V])(implicit ev: T <:< IterableOnce[V]): CondExp[T] = liftCondExp {
    ExpAlg.ForAll[T, V, Boolean](ev, cond, True, False)
  }

  def exists[T, V](cond: CondExp[V])(implicit ev: T <:< IterableOnce[V]): CondExp[T] = liftCondExp {
    ExpAlg.Exists[T, V, Boolean](ev, cond, True, False)
  }

  def in[T](set: Set[T]): CondExp[T] = liftCondExp {
    ExpAlg.SetContains(set, True, False)
  }

  def within[T](window: Window[T]): CondExp[T] = liftCondExp {
    ExpAlg.Within[T, Boolean](window, True, False)
  }

  def equalTo[T : Eq](value: T): CondExp[T] = liftCondExp {
    ExpAlg.EqualTo[T, Boolean](value, True, False)
  }

  // TODO: Figure out how to use this conditional tertiary expression

  def when[T, A](exp: Exp[T, Boolean])(thenExp: Exp[T, A])(elseExp: Exp[T, A]): Exp[T, A] = liftExp {
    ExpAlg.Cond[T, A](exp, thenExp, elseExp)
  }

  def and[T, A : Intersect](
    one: Exp[T, A],
    two: Exp[T, A],
    others: Exp[T, A]*,
  ): Exp[T, A] = liftExp {
    ExpAlg.And[T, A](
      Intersect[A].intersect,
      one :: two :: others.toList,
    )
  }

  def or[T, A : Union](
    one: Exp[T, A],
    two: Exp[T, A],
    others: Exp[T, A]*,
  ): Exp[T, A] = liftExp {
    ExpAlg.Or[T, A](
      Union[A].union,
      one :: two :: others.toList,
    )
  }

  def alwaysTrue[T]: CondExp[T] = liftCondExp(ExpAlg.Pure("True", _ => true))

  def alwaysFalse[T]: CondExp[T] = liftCondExp(ExpAlg.Pure("False", _ => false))

  def alwaysMatch: TerminalFactsExp = liftAnyTermExp(ExpAlg.Pure("AlwaysMatch", FactsMatch(_)))

  def alwaysEmpty: TerminalFactsExp = liftAnyTermExp(ExpAlg.Pure("AlwaysEmpty", _ => NoFactsMatch()))

  // Helper methods for building conditional expressions that terminate in a functor to boolean
  private def True[X]: X => Boolean = _ => true
  private def False[X]: X => Boolean = _ => false

  /** @see [[TerminalFactsExp]] */
  private def liftAnyTermExp(exp: ExpAlg[Facts, ResultSet]): TerminalFactsExp =
    FreeApplicative.lift(exp)

  /** @see [[CondExp]] */
  private def liftCondExp[X](exp: ExpAlg[X, Boolean]): CondExp[X] = FreeApplicative.lift(exp)

  /** @see [[Exp]] */
  private def liftExp[X, A](exp: ExpAlg[X, A]): Exp[X, A] = FreeApplicative.lift(exp)
}
