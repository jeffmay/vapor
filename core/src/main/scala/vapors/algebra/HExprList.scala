package com.rallyhealth

package vapors.algebra

import cats.data.Ior
import cats.syntax.all._
import cats.{Align, Functor, FunctorFilter, Id, Semigroupal}
import shapeless.{::, HList, HNil}

/**
  * A helpful short-hand for an HList of expressions that all share the same input, effect, and parameter types.
  *
  * This not only provides syntactic sugar by fixing some of the type arguments to avoid repetition, it also
  * provides implementation for some recursive operations that produce HList outputs.
  */
sealed trait NonEmptyExprHList[V, M[_], L <: HList, P] {

  /**
    * Prepends an expression that accepts the same input and outputs a value wrapped in the same effect as
    * all the other expressions.
    */
  def ::[H](other: Expr[V, M[H], P]): NonEmptyExprHList[V, M, H :: L, P] =
    new ExprHCons[V, M, H, L, P](other, this)

  /**
    * Visits each expression in the HList, applies the given visitor, and combines the results by applying a
    * product operation on the effectful container of results to produce every possible combination of HList
    * from the given input.
    *
    * @note If given the [[Id]] effect, this is equivalent to mapping over each given expression to produce
    *       an expression that takes the same input and produces a single HList combining every result type.
    *
    * @example NonEmptyExprHList.of(const("A"), const(1)).visitProduct(g) == const("A" :: 1 :: HNil).visit(g)
    *
    * @note If given a [[List]], this will combine the resulting lists of each expression into a list of
    *       every permutation of HList that can be produced from the output lists.
    *
    * @example NonEmptyExprHList.of(const(List("A", "B")), const(List(1, 2))).visitProduct(g) == const(
    *            List("A" :: 1 :: HNil, "A" :: 2 :: HNil, "B" :: 1 :: HNil, "C" :: 2 :: HNil)
    *          ).visit(g)
    *
    * @example NonEmptyExprHList.of(const(List("A")), const(Nil)).visitProduct(g) == const(Nil).visit(g)
    */
  def visitProduct[G[_] : Functor : Semigroupal](
    v: Expr.Visitor[V, P, G],
  )(implicit
    functorM: Functor[M],
    semigroupalM: Semigroupal[M],
  ): G[M[L]]

  /**
    * Visits each expression in the HList, applies the given visitor, zips over every element in the resulting
    * functor-wrapped values (stopping at the length of the first sequence to run out of elements), and then
    * combines the elements into an HList.
    *
    * @example NonEmptyExprHList.of(const(List("A", "B", "C"), const(List(1, 2, 3, 4, 5))).visitZippedToShortest(g) ==
    *            const(List("A" :: 1 :: HNil, "B" :: 2 :: HNil, "C" :: 3 :: HNil)).visit(g)
    *
    * @example NonEmptyExprHList.of(const(List("A", "B", "C"), const(Nil)).visitZippedToShortest(g) ==
    *            const(Nil).visit(g)
    */
  def visitZippedToShortest[G[_] : Functor : Semigroupal](
    v: Expr.Visitor[V, P, G],
  )(implicit
    alignM: Align[M],
    filterM: FunctorFilter[M],
  ): G[M[L]]
}

object NonEmptyExprHList {

  def tail[V, R, P](expr: Expr[V, R, P]): ExprLast[V, Id, R, P] =
    ExprLast[V, Id, R, P](expr)

  def tailK[V, M[_], R, P](expr: Expr[V, M[R], P]): ExprLast[V, M, R, P] =
    ExprLast(expr)
}

final case class ExprHCons[V, M[_], H, T <: HList, P](
  head: Expr[V, M[H], P],
  tail: NonEmptyExprHList[V, M, T, P],
) extends NonEmptyExprHList[V, M, H :: T, P] {

  override def visitProduct[G[_] : Functor : Semigroupal](
    v: Expr.Visitor[V, P, G],
  )(implicit
    functorM: Functor[M],
    semigroupalM: Semigroupal[M],
  ): G[M[H :: T]] = {
    Semigroupal[G].product(head.visit(v), tail.visitProduct(v)).map {
      _.mapN(_ :: _)
    }
  }

  override def visitZippedToShortest[G[_] : Functor : Semigroupal](
    v: Expr.Visitor[V, P, G],
  )(implicit
    alignM: Align[M],
    filterM: FunctorFilter[M],
  ): G[M[H :: T]] = {
    Semigroupal[G].product(head.visit(v), tail.visitZippedToShortest(v)).map {
      case (hs, ts) =>
        Align[M]
          .alignWith(hs, ts) {
            case Ior.Both(h, t) => Some(h :: t)
            case _ => None
          }
          .collect {
            case Some(hlist) => hlist
          }
    }
  }
}

final case class ExprLast[V, M[_], H, P](last: Expr[V, M[H], P]) extends NonEmptyExprHList[V, M, H :: HNil, P] {

  override def visitProduct[G[_] : Functor : Semigroupal](
    v: Expr.Visitor[V, P, G],
  )(implicit
    functorM: Functor[M],
    semigroupalM: Semigroupal[M],
  ): G[M[H :: HNil]] = {
    last.visit(v).map(_.map(_ :: HNil))
  }

  override def visitZippedToShortest[G[_] : Functor : Semigroupal](
    v: Expr.Visitor[V, P, G],
  )(implicit
    alignM: Align[M],
    filterM: FunctorFilter[M],
  ): G[M[H :: HNil]] = {
    last.visit(v).map(filterM.functor.map(_)(_ :: HNil))
  }
}
