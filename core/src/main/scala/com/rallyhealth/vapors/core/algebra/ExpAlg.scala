package com.rallyhealth.vapors.core.algebra

import cats.free.FreeApplicative
import com.rallyhealth.vapors.core.data.{NamedLens, Window}

sealed trait ExpAlg[T, A]

object ExpAlg {

  final case class Pure[T, A](
    label: String,
    always: T => A,
  ) extends ExpAlg[T, A]

  final case class Select[T, U, A](
    selector: NamedLens[T, U],
    expression: FreeApplicative[ExpAlg[U, *], A],
  ) extends ExpAlg[T, A]

  final case class ForAll[T, U, A](
    toIterable: T => IterableOnce[U],
    condition: FreeApplicative[ExpAlg[U, *], Boolean],
    whenTrue: T => A,
    whenFalse: T => A,
  ) extends ExpAlg[T, A]

  final case class Exists[T, U, A](
    toIterable: T => IterableOnce[U],
    condition: FreeApplicative[ExpAlg[U, *], Boolean],
    whenTrue: T => A,
    whenFalse: T => A,
  ) extends ExpAlg[T, A]

  final case class Within[T, A](
    window: Window[T],
    whenTrue: T => A,
    whenFalse: T => A,
  ) extends ExpAlg[T, A]

  // TODO: Use partial function?
  // TODO: Less generic name for query language
  final case class Collect[T, U, A](
    subtypeName: String,
    collect: T => Option[U],
    expression: FreeApplicative[ExpAlg[U, *], A],
    whenEmpty: T => A,
  ) extends ExpAlg[T, A]

  final case class Cond[T, A](
    condition: FreeApplicative[ExpAlg[T, *], Boolean],
    thenExpression: FreeApplicative[ExpAlg[T, *], A],
    elseExpression: FreeApplicative[ExpAlg[T, *], A],
  ) extends ExpAlg[T, A]

  final case class And[T, A](
    combine: List[A] => A,
    expressions: List[FreeApplicative[ExpAlg[T, *], A]],
  ) extends ExpAlg[T, A]

  final case class Or[T, A](
    combine: List[A] => A,
    expressions: List[FreeApplicative[ExpAlg[T, *], A]],
  ) extends ExpAlg[T, A]
}
