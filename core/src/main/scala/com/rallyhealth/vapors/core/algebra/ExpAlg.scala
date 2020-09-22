package com.rallyhealth.vapors.core.algebra

import cats.free.FreeApplicative
import com.rallyhealth.vapors.core.data.{NamedLens, Window}

// TODO: Use CursorAlg style for ADT naming
sealed trait ExpAlg[T, A]

final case class ExpPure[T, A](
  label: String,
  always: T => A,
) extends ExpAlg[T, A]

final case class ExpSelectField[T, U, A](
  selector: NamedLens[T, U],
  expression: FreeApplicative[ExpAlg[U, *], A],
) extends ExpAlg[T, A]

final case class ExpForAll[T, U, A](
  toIterable: T => IterableOnce[U],
  condition: FreeApplicative[ExpAlg[U, *], Boolean],
  whenTrue: T => A,
  whenFalse: T => A,
) extends ExpAlg[T, A]

final case class ExpExists[T, U, A](
  toIterable: T => IterableOnce[U],
  condition: FreeApplicative[ExpAlg[U, *], Boolean],
  whenTrue: T => A,
  whenFalse: T => A,
) extends ExpAlg[T, A]

final case class ExpWithin[T, A](
  window: Window[T],
  whenTrue: T => A,
  whenFalse: T => A,
) extends ExpAlg[T, A]

// TODO: Use partial function?
// TODO: Less generic name for query language
final case class ExpCollect[T, U, A](
  subtypeName: String,
  collect: T => Option[U],
  expression: FreeApplicative[ExpAlg[U, *], A],
  whenEmpty: T => A,
) extends ExpAlg[T, A]

final case class ExpCond[T, A](
  condition: FreeApplicative[ExpAlg[T, *], Boolean],
  thenExpression: FreeApplicative[ExpAlg[T, *], A],
  elseExpression: FreeApplicative[ExpAlg[T, *], A],
) extends ExpAlg[T, A]

final case class ExpAnd[T, A](
  combine: List[A] => A,
  expressions: List[FreeApplicative[ExpAlg[T, *], A]],
) extends ExpAlg[T, A]

final case class ExpOr[T, A](
  combine: List[A] => A,
  expressions: List[FreeApplicative[ExpAlg[T, *], A]],
) extends ExpAlg[T, A]
