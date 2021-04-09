package com.rallyhealth.vapors.core.lens

import cats.arrow.Compose
import cats.data.NonEmptySet
import cats.kernel.Semigroup
import com.rallyhealth.vapors.core.lens.NamedLens.AsIterableBuilder
import shapeless.ops.hlist
import shapeless.{Generic, HList}

import scala.collection.{Factory, MapView, View}
import scala.reflect.ClassTag

object NamedLens extends NamedLensLowPriorityImplicits {

  type Id[A] = NamedLens[A, A]
  val Id: NamedLens[Any, Any] = NamedLens(DataPath.empty, identity[Any])
  def id[A]: NamedLens[A, A] = Id.asInstanceOf[Id[A]]

  type Fn[A, B] = NamedLens.Id[A] => NamedLens[A, B]

  implicit object ComposeInstance extends Compose[NamedLens] {
    override def compose[A, B, C](
      f: NamedLens[B, C],
      g: NamedLens[A, B],
    ): NamedLens[A, C] =
      g.andThen(f)
  }

  implicit final class Selector[A, B](val lens: NamedLens[A, B]) extends AnyVal {
    def select[C](getter: B => C): NamedLens[A, C] = macro NamedLensMacros.selectImpl[A, B, C]
  }

  implicit def asMap[A, B, C[x] <: IterableOnce[x], K, E](
    lens: NamedLens[A, B],
  )(implicit
    ev: B <:< C[(K, E)],
  ): AsMapBuilder[A, C, K, E] =
    new AsMapBuilder[A, C, K, E](lens.as[C[(K, E)]])

  final class AsMapBuilder[A, C[x] <: IterableOnce[x], K, V](private val lens: NamedLens[A, C[(K, V)]]) extends AnyVal {

    def values: NamedLens[A, Iterable[V]] = lens.copy(
      get = lens.get.andThen(_.iterator.to(View).map(_._2)),
    )

    // For some reason AsIterableBuilder does not pick up a MapBuilder as an IterableBuilder

    def to[B](factory: Factory[(K, V), B]): NamedLens[A, B] =
      lens.copy(get = lens.get.andThen(factory.fromSpecific(_)))

    def toMap: NamedLens[A, Map[K, V]] = lens.copy(
      get = lens.get.andThen(Map.from(_)),
    )

    def toMapView: NamedLens[A, MapView[K, V]] = lens.copy(
      get = lens.get.andThen(Map.from(_).view),
    )
  }

  // TODO: Make this more generally useful and convert back to a NamedLens implicitly
  final class AsIterableBuilder[A, C[x] <: IterableOnce[x], E](private val lens: NamedLens[A, C[E]]) extends AnyVal {

    // TODO: Should there be any path information for conversion?
    //       List => Map seems important information as values with duplicate keys could be dropped
    def to[B : ClassTag](factory: Factory[E, B]): NamedLens[A, B] =
      lens.copy(
        path = lens.path.to(factory),
        get = lens.get.andThen(factory.fromSpecific(_)),
      )

    def headOption: NamedLens[A, Option[E]] =
      lens.copy(
        path = lens.path.atHead,
        get = lens.get.andThen(b => Iterable.from[E](b).headOption),
      )
  }

  implicit final class AsHListBuilder[A, L <: HList](private val lens: NamedLens[A, L]) extends AnyVal {

    def tupled[T](implicit tupler: hlist.Tupler.Aux[L, T]): NamedLens[A, T] =
      lens.copy(
        path = lens.path,
        get = lens.get.andThen(tupler.apply),
      )
  }
}

sealed trait NamedLensLowPriorityImplicits {

  implicit def asIterable[A, B, C[x] <: IterableOnce[x], E](
    lens: NamedLens[A, B],
  )(implicit
    ev: B <:< C[E],
  ): AsIterableBuilder[A, C, E] =
    new AsIterableBuilder(lens.as[C[E]])
}

final case class NamedLens[A, B](
  path: DataPath,
  get: A => B,
) { outer =>

  // Because extension methods don't retain type information well, this is still useful despite the Compose instance
  def andThen[C](lens: NamedLens[B, C]): NamedLens[A, C] = {
    copy(
      path = this.path ++ lens.path,
      get = get.andThen(lens.get),
    )
  }

  // Helpful for building a tuple with the lens itself. Identical to identity, but you don't have to specify the type.
  def self: NamedLens[A, B] = this

  // TODO: Use HList to make this more safe, instead of relying on the macro to use this properly.
  def field[C](
    name: String,
    getter: B => C,
  ): NamedLens[A, C] = {
    copy(
      path = path.atField(name),
      get = this.get.andThen(getter),
    )
  }

  def at[K : ValidDataPathKey, V](
    key: K,
  )(implicit
    CI: Indexed[B, K, V],
  ): NamedLens[A, V] = {
    copy(
      path = path.atKey(key),
      get = get.andThen(b => CI.get(b)(key)),
    )
  }

  def filterKeys[K : ValidDataPathKey, V : Semigroup](
    keys: NonEmptySet[K],
  )(implicit
    CI: Indexed[B, K, V],
  ): NamedLens[A, V] = {
    import com.rallyhealth.vapors.core.syntax.indexed._
    copy(
      path = path.filterKeys(keys),
      get = get.andThen(_.filterKeys(keys)),
    )
  }

  def as[V](implicit ev: B <:< V): NamedLens[A, V] =
    this.copy(get = this.get.andThen(ev))

  // TODO: Are these runtime down casts needed? Why not just use .asInstanceOf (with a safe wrapper)

  def asIterable[V](implicit ev: B <:< IterableOnce[V]): NamedLens.AsIterableBuilder[A, IterableOnce, V] =
    new NamedLens.AsIterableBuilder(this.copy(get = this.get.andThen(ev)))

  def asMap[K, V](implicit ev: B <:< IterableOnce[(K, V)]): NamedLens.AsMapBuilder[A, IterableOnce, K, V] =
    new NamedLens.AsMapBuilder(this.copy(get = this.get.andThen(ev)))

  def asHList[L <: HList](implicit gen: Generic.Aux[B, L]): NamedLens[A, L] = copy(get = this.get.andThen(gen.to))

}
