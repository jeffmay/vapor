package com.rallyhealth

package vapors.lens

import vapors.lens.NamedLens.AsIterableBuilder

import cats.arrow.Compose
import cats.data.NonEmptySet
import cats.kernel.Semigroup
import shapeless.ops.hlist
import shapeless.{Generic, HList}

import scala.collection.{Factory, MapView, View}

/**
  * @see [[NamedLens]]
  */
object NamedLens extends NamedLensLowPriorityImplicits {

  /**
    * The type of lens that returns the value it is given.
    */
  type Id[A] = NamedLens[A, A]

  /**
    * A singleton implementation of the [[identity]] lens.
    */
  val Id: NamedLens[Any, Any] = NamedLens(DataPath.empty, identity[Any])

  /**
    * The only definition of the [[Id]] lens.
    */
  def id[A]: NamedLens[A, A] = Id.asInstanceOf[Id[A]]

  /**
    * A function for building a lens from the [[Id]] lens.
    */
  type Fn[A, B] = NamedLens.Id[A] => NamedLens[A, B]

  implicit object ComposeInstance extends Compose[NamedLens] {
    override def compose[A, B, C](
      f: NamedLens[B, C],
      g: NamedLens[A, B],
    ): NamedLens[A, C] =
      g.andThen(f)
  }

  /**
    * A fancy trick using macros that allows you to access the surrounding context of a method
    * invocation. By wrapping the call with this class, we can access the given [[lens]] to
    * apply the appropriate transformation.
    *
    * @param lens the lens to add the `.select` method to.
    */
  implicit final class Selector[A, B](val lens: NamedLens[A, B]) extends AnyVal {

    /**
      * Create a [[NamedLens]] that selects a field based on a given function and applies the appropriate
      * [[DataPath.atField]] operator.
      *
      * @note this uses a macro and only works for `val`s or `def`s with no arguments.
      *
      * @param getter a function that selects a value of type [[C]] from the return type [[B]] of the current lens
      */
    def select[C](getter: B => C): NamedLens[A, C] = macro NamedLensMacros.selectImpl[A, B, C]
  }

  /**
    * If the lens returns a [[IterableOnce]] of 2-tuples, then allow calling operations defined by [[AsMapBuilder]]
    */
  implicit def asMap[A, B, C[x] <: IterableOnce[x], K, E](
    lens: NamedLens[A, B],
  )(implicit
    ev: B <:< C[(K, E)],
  ): AsMapBuilder[A, C, K, E] =
    new AsMapBuilder[A, C, K, E](lens.as[C[(K, E)]])

  final class AsMapBuilder[A, C[x] <: IterableOnce[x], K, V](private val lens: NamedLens[A, C[(K, V)]]) extends AnyVal {

    /**
      * Returns the right-side of all the 2-tuples as an [[Iterable]] of values.
      */
    def values: NamedLens[A, Iterable[V]] = lens.copy(
      get = lens.get.andThen(_.iterator.to(View).map(_._2)),
    )

    /**
      * @see [[AsIterableBuilder.to]] but the values are 2-tuples
      */
    def to[B](factory: Factory[(K, V), B]): NamedLens[A, B] =
      lens.copy(get = lens.get.andThen(factory.fromSpecific(_)))

    /**
      * Group the 2-tuples by the left side and create a [[Map]].
      *
      * If there are duplicate keys, then the last entry in the sequence wins the spot and the others are dropped.
      */
    def toMap: NamedLens[A, Map[K, V]] = lens.copy(
      get = lens.get.andThen(Map.from(_)),
    )

    /**
      * Same as [[toMap]], but creates a lazy [[MapView]] instead.
      */
    def toMapView: NamedLens[A, MapView[K, V]] = lens.copy(
      get = lens.get.andThen(Map.from(_).view),
    )
  }

  // TODO: Make this more generally useful and convert back to a NamedLens implicitly
  final class AsIterableBuilder[A, C[x] <: IterableOnce[x], E](private val lens: NamedLens[A, C[E]]) extends AnyVal {

    /**
      * Use the given Scala collection companion object to convert this [[IterableOnce]] into the desired output.
      *
      * TODO: Should there be any path information for conversion?
      *       List => Map seems important information as values with duplicate keys could be dropped
      */
    def to[B](factory: Factory[E, B]): NamedLens[A, B] =
      lens.copy(get = lens.get.andThen(factory.fromSpecific(_)))

    /**
      * Get the first element of this [[IterableOnce]], if it isn't empty.
      */
    def headOption: NamedLens[A, Option[E]] =
      lens.copy(
        path = lens.path.atHead,
        get = lens.get.andThen(b => View.from[E](b).headOption),
      )
  }

  implicit final class AsHListBuilder[A, L <: HList](private val lens: NamedLens[A, L]) extends AnyVal {

    /**
      * Convert the resulting [[HList]] into a tuple.
      */
    def tupled[T](implicit tupler: hlist.Tupler.Aux[L, T]): NamedLens[A, T] =
      lens.copy(
        path = lens.path,
        get = lens.get.andThen(tupler.apply),
      )
  }
}

sealed trait NamedLensLowPriorityImplicits {

  /**
    * Wrap the result of this lens with an [[AsIterableBuilder]] for helper operations on [[IterableOnce]] types.
    */
  implicit def asIterable[A, B, C[x] <: IterableOnce[x], E](
    lens: NamedLens[A, B],
  )(implicit
    ev: B <:< C[E],
  ): AsIterableBuilder[A, C, E] =
    new AsIterableBuilder(lens.as[C[E]])
}

/**
  * A serializable lens for extracting a value from an object.
  *
  * @param path the serializable path to the value from the starting type to the value type
  * @param get a function for extracting the field value
  * @tparam A the starting type
  * @tparam B the extracted value type
  */
final case class NamedLens[A, B](
  path: DataPath,
  get: A => B,
) { outer =>

  /**
    * Composes another lens to run on the output of this lens.
    *
    * @note Because extension methods don't retain type information well,
    *       this is still useful despite the [[Compose]] instance.
    */
  def andThen[C](lens: NamedLens[B, C]): NamedLens[A, C] = {
    copy(
      path = this.path ++ lens.path,
      get = get.andThen(lens.get),
    )
  }

  // Helpful for building a tuple with the lens itself. Identical to identity, but you don't have to specify the type.
  def self: NamedLens[A, B] = this

  /**
    * Access a field from the object returned by this lens.
    *
    * Typically, you would not call this directly, but instead by calling the [[NamedLens.Selector.select]]
    * extension method on this lens. The `.select` method uses macros to make sure that the `name` param is
    * set properly.
    *
    * @param name the name of the field
    * @param getter a function to extract the value of this field from the object
    * TODO: Use HList to make this more safe, instead of relying on the macro to use this properly.
    */
  def field[C](
    name: String,
    getter: B => C,
  ): NamedLens[A, C] = {
    copy(
      path = path.atField(name),
      get = this.get.andThen(getter),
    )
  }

  /**
    * Used for accessing the value for a given key in this object.
    *
    * This requires that there is an [[Indexed]] definition for how to access a value from the
    * indexable object returned by the current lens.
    *
    * @param key either a value of the appropriate key type OR a [[shapeless.Nat]] for accessing a
    *            specifically typed value from a tuple or [[HList]]
    */
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

  /**
    * Filters the given set of keys from this indexable object.
    *
    * @note Unfortunately this does not work for [[HList]] the same way that [[at]] does.
    *
    * TODO: It may be possible to make this work for tuples / HList with varargs instead of [[NonEmptySet]].
    */
  def filterKeys[K : ValidDataPathKey, V : Semigroup](
    keys: NonEmptySet[K],
  )(implicit
    CI: Indexed[B, K, V],
  ): NamedLens[A, V] = {
    import vapors.syntax.indexed._
    copy(
      path = path.filterKeys(keys),
      get = get.andThen(_.filterKeys(keys)),
    )
  }

  /**
    * Upcasts the lens.
    *
    * @note this would not be necessary if [[NamedLens]] used appropriate variance, however, variance for lenses
    *       introduces a lot of issues.
    */
  def as[V](implicit ev: B <:< V): NamedLens[A, V] =
    this.copy(get = this.get.andThen(ev))

  /**
    * Converts the result into an [[HList]] if possible.
    *
    * Uses recursive implicit resolution at compile-time to derive the appropriate heterogenious return type.
    *
    * @see [[Generic]]
    */
  def asHList[L <: HList](implicit gen: Generic.Aux[B, L]): NamedLens[A, L] = copy(get = this.get.andThen(gen.to))

  // TODO: Are these runtime down casts needed? Why not just use .asInstanceOf (with a safe wrapper)

  def asIterable[V](implicit ev: B <:< IterableOnce[V]): NamedLens.AsIterableBuilder[A, IterableOnce, V] =
    new NamedLens.AsIterableBuilder(this.copy(get = this.get.andThen(ev)))

  def asMap[K, V](implicit ev: B <:< IterableOnce[(K, V)]): NamedLens.AsMapBuilder[A, IterableOnce, K, V] =
    new NamedLens.AsMapBuilder(this.copy(get = this.get.andThen(ev)))

}
