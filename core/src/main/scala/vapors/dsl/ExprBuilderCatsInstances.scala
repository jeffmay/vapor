package com.rallyhealth

package vapors.dsl

import cats._
import cats.data.Ior
import cats.syntax.all._

import scala.collection.{MapView, View}

trait ExprBuilderCatsInstances {

  implicit def monadSet: Monad[Set] = alleycats.std.set.alleyCatsStdSetMonad

  implicit def traverseSet: Traverse[Set] = alleycats.std.set.alleyCatsSetTraverse

  implicit def traverseFilterSet: TraverseFilter[Set] = alleycats.std.set.alleyCatsSetTraverseFilter

  implicit def traverseMap[K]: Traverse[Map[K, *]] = alleycats.std.map.alleycatsStdInstancesForMap

  implicit def traverseFilterMap[K]: TraverseFilter[Map[K, *]] = alleycats.std.map.alleycatsStdMapTraverseFilter

  implicit def traverseMapView[K]: Traverse[MapView[K, *]] = new Traverse[MapView[K, *]] {

    override final def traverse[G[_], A, B](
      fa: MapView[K, A],
    )(
      f: A => G[B],
    )(implicit
      G: Applicative[G],
    ): G[MapView[K, B]] =
      traverseMap.traverse(fa.toMap)(f).map(_.view)

    override final def map[A, B](fa: MapView[K, A])(f: A => B): MapView[K, B] = fa.mapValues(f)

    override final def foldLeft[A, B](
      fa: MapView[K, A],
      b: B,
    )(
      f: (B, A) => B,
    ): B =
      fa.foldLeft(b) { case (x, (_, a)) => f(x, a) }

    override final def foldRight[A, B](
      fa: MapView[K, A],
      lb: Eval[B],
    )(
      f: (A, Eval[B]) => Eval[B],
    ): Eval[B] =
      Foldable.iterateRight(fa.values, lb)(f)

    override final def size[A](fa: MapView[K, A]): Long = fa.size.toLong

    override final def get[A](fa: MapView[K, A])(idx: Long): Option[A] = {
      if (idx < 0L || Int.MaxValue < idx) None
      else {
        val n = idx.toInt
        if (n >= fa.size) None
        else Some(fa.valuesIterator.drop(n).next())
      }
    }

    override final def isEmpty[A](fa: MapView[K, A]): Boolean = fa.isEmpty

    override final def fold[A](fa: MapView[K, A])(implicit A: Monoid[A]): A = A.combineAll(fa.values)

    override final def toList[A](fa: MapView[K, A]): List[A] = fa.values.toList

    override final def collectFirst[A, B](fa: MapView[K, A])(pf: PartialFunction[A, B]): Option[B] =
      fa.collectFirst(new PartialFunction[(K, A), B] {
        override final def isDefinedAt(x: (K, A)): Boolean = pf.isDefinedAt(x._2)
        override final def apply(v1: (K, A)): B = pf(v1._2)
      })

    override final def collectFirstSome[A, B](fa: MapView[K, A])(f: A => Option[B]): Option[B] =
      collectFirst(fa)(Function.unlift(f))
  }

  implicit def traverseFilterMapView[K]: TraverseFilter[MapView[K, *]] = new TraverseFilter[MapView[K, *]] {
    override final def traverse: Traverse[MapView[K, *]] = traverseMapView
    override final def traverseFilter[G[_], A, B](
      fa: MapView[K, A],
    )(
      f: A => G[Option[B]],
    )(implicit
      G: Applicative[G],
    ): G[MapView[K, B]] = {
      import cats.syntax.functor._
      traverseFilterMap.traverseFilter(fa.toMap)(f).map(_.view)
    }
  }

  implicit val vaporsInstancesForView: Traverse[View]
    with TraverseFilter[View]
    with Alternative[View]
    with Monad[View]
    with CoflatMap[View]
    with Align[View] =
    new Traverse[View] with TraverseFilter[View] with Alternative[View] with Monad[View] with CoflatMap[View]
    with Align[View] {

      override final def traverse: Traverse[View] = this

      override final def empty[A]: View[A] = View.empty

      override final def combineK[A](
        x: View[A],
        y: View[A],
      ): View[A] = x ++ y

      override final def pure[A](x: A): View[A] = View(x)

      override final def map[A, B](fa: View[A])(f: A => B): View[B] =
        fa.map(f)

      override final def flatMap[A, B](fa: View[A])(f: A => View[B]): View[B] =
        fa.flatMap(f)

      override final def map2[A, B, Z](
        fa: View[A],
        fb: View[B],
      )(
        f: (A, B) => Z,
      ): View[Z] =
        if (fb.isEmpty) View.empty // do O(1) work if fb is empty
        else fa.flatMap(a => fb.map(b => f(a, b))) // already O(1) if fa is empty

      override final def map2Eval[A, B, Z](
        fa: View[A],
        fb: Eval[View[B]],
      )(
        f: (A, B) => Z,
      ): Eval[View[Z]] =
        if (fa.isEmpty) Eval.now(View.empty) // no need to evaluate fb
        else fb.map(fb => map2(fa, fb)(f))

      override final def coflatMap[A, B](fa: View[A])(f: View[A] => B): View[B] =
        fa.tails.to(View).init.map(f)

      override final def foldLeft[A, B](
        fa: View[A],
        b: B,
      )(
        f: (B, A) => B,
      ): B =
        fa.foldLeft(b)(f)

      override final def foldRight[A, B](
        fa: View[A],
        lb: Eval[B],
      )(
        f: (A, Eval[B]) => Eval[B],
      ): Eval[B] =
        Now(fa).flatMap { s =>
          // Note that we don't use pattern matching, since that may needlessly force the tail.
          if (s.isEmpty) lb else f(s.head, Eval.defer(foldRight(s.tail, lb)(f)))
        }

      override final def foldMap[A, B](fa: View[A])(f: A => B)(implicit B: Monoid[B]): B =
        B.combineAll(fa.iterator.map(f))

      override final def traverse[G[_], A, B](fa: View[A])(f: A => G[B])(implicit G: Applicative[G]): G[View[B]] =
        Traverse[LazyList].traverse(fa.to[LazyList[A]](LazyList))(f).map(_.to(View))

      override final def mapWithIndex[A, B](fa: View[A])(f: (A, Int) => B): View[B] =
        fa.zipWithIndex.map(ai => f(ai._1, ai._2))

      override final def zipWithIndex[A](fa: View[A]): View[(A, Int)] =
        fa.zipWithIndex

      override final def tailRecM[A, B](a: A)(fn: A => View[Either[A, B]]): View[B] =
        FlatMap[LazyList]
          .tailRecM(a) {
            fn.andThen(_.to[LazyList[Either[A, B]]](LazyList))
          }
          .to(View)

      override final def exists[A](fa: View[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override final def forall[A](fa: View[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override final def get[A](fa: View[A])(idx: Long): Option[A] =
        Traverse[LazyList].get(fa.to[LazyList[A]](LazyList))(idx)

      override final def isEmpty[A](fa: View[A]): Boolean = fa.isEmpty

      override final def foldM[G[_], A, B](
        fa: View[A],
        z: B,
      )(
        f: (B, A) => G[B],
      )(implicit
        G: Monad[G],
      ): G[B] =
        Traverse[LazyList].foldM(fa.to[LazyList[A]](LazyList), z)(f)

      override final def fold[A](fa: View[A])(implicit A: Monoid[A]): A = A.combineAll(fa)

      override final def toList[A](fa: View[A]): List[A] = fa.toList

      override final def toIterable[A](fa: View[A]): Iterable[A] = fa

      override def reduceLeftOption[A](fa: View[A])(f: (A, A) => A): Option[A] =
        fa.reduceLeftOption(f)

      override final def find[A](fa: View[A])(f: A => Boolean): Option[A] = fa.find(f)

      override final def algebra[A]: Monoid[View[A]] = new Monoid[View[A]] {
        override final def empty: View[A] = View.empty
        override final def combine(
          x: View[A],
          y: View[A],
        ): View[A] = x ++ y
      }

      override final def collectFirst[A, B](fa: View[A])(pf: PartialFunction[A, B]): Option[B] = fa.collectFirst(pf)

      override final def collectFirstSome[A, B](fa: View[A])(f: A => Option[B]): Option[B] =
        fa.collectFirst(Function.unlift(f))

      override final def align[A, B](
        fa: View[A],
        fb: View[B],
      ): View[Ior[A, B]] =
        alignWith(fa, fb)(identity)

      override final def alignWith[A, B, C](
        fa: View[A],
        fb: View[B],
      )(
        f: Ior[A, B] => C,
      ): View[C] =
        Align[LazyList].alignWith(fa.to[LazyList[A]](LazyList), fb.to[LazyList[B]](LazyList))(f).to(View)

      override final def traverseFilter[G[_], A, B](
        fa: View[A],
      )(
        f: A => G[Option[B]],
      )(implicit
        G: Applicative[G],
      ): G[View[B]] =
        TraverseFilter[LazyList].traverseFilter(fa.to[LazyList[A]](LazyList))(f).map(_.to(View))

      override def filterA[G[_], A](fa: View[A])(f: A => G[Boolean])(implicit G: Applicative[G]): G[View[A]] =
        TraverseFilter[LazyList].filterA(fa.to[LazyList[A]](LazyList))(f).map(_.to(View))

      override def mapFilter[A, B](fa: View[A])(f: A => Option[B]): View[B] =
        fa.collect(Function.unlift(f))
    }
}
