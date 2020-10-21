package com.rallyhealth.vapors.core.data

import com.rallyhealth.vapors.core.macros.NamedLensMacros

import scala.reflect.runtime.universe.TypeTag

case class NamedLens[A, B](
  path: DataPath,
  get: A => B,
) {

  def compose[C](lens: NamedLens[C, A]): NamedLens[C, B] = {
    copy(
      path = lens.path ::: this.path,
      get = get.compose(lens.get),
    )
  }

  def andThen[C](lens: NamedLens[B, C]): NamedLens[A, C] = {
    copy(
      path = lens.path ::: this.path, // TODO: Why is this broken?
      get = get.andThen(lens.get),
    )
  }

  // TODO: Use macro to guarantee safety here
  def as[C : TypeTag](f: B => C): NamedLens[A, C] = copy(get = this.get.andThen(f))

  def field[C](
    name: String,
    getter: B => C,
  ): NamedLens[A, C] = {
    copy(
      path = path.atField(name),
      get = this.get.andThen(getter),
    )
  }

  def atKey[C[x, y] <: Map[x, y], K : ValidDataPathKey, V](
    key: K,
  )(implicit
    ev: B <:< C[K, V],
  ): NamedLens[A, Option[V]] = {
    copy(
      path = path.atKey(key),
      get = get.andThen(b => ev(b).get(key)),
    )
  }

  def head[C[x] <: Iterable[x], V](implicit ev: B <:< C[V]): NamedLens[A, Option[V]] = {
    copy(
      path = path.atHead,
      get = get.andThen(b => ev(b).headOption),
    )
  }
}

object NamedLens {

  type Id[A] = NamedLens[A, A]
  val Id: NamedLens[Any, Any] = NamedLens(DataPath(Nil), identity[Any])
  def id[A]: NamedLens[A, A] = Id.asInstanceOf[Id[A]]

  implicit class Selector[A, B](val lens: NamedLens[A, B]) extends AnyVal {
    def select[C](getter: B => C): NamedLens[A, C] = macro NamedLensMacros.selectImpl[A, B, C]
  }

}
