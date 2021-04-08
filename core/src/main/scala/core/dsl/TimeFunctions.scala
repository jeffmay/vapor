package com.rallyhealth.vapors

package core.dsl

import core.algebra.{CaptureP, Expr}
import core.data.FactTable
import core.time.{CountTime, ModifyTime}

import shapeless._

import java.time.{Clock, Instant, LocalDate}

trait TimeFunctions {

  // TODO: Can this be converted to Addition[T] if we make the second argument a different type? Maybe AddLeft[T, D]?
  final def dateAdd[V, T, D, P](
    lhs: Expr[V, T, P],
    rhs: Expr[V, D, P],
  )(implicit
    modTime: ModifyTime[T, D],
    argTypeInfo: Typeable[T :: D :: HNil],
    captureArgResult: CaptureP[V, T :: D :: HNil, P],
    captureResult: CaptureP[V, T, P],
  ): Expr[V, T, P] = {
    val argExpr = wrap(lhs, rhs).asHList.returnOutput
    val fn: T :: D :: HNil => T = {
      case lhs :: rhs :: HNil =>
        ModifyTime.addTo(lhs, rhs)
    }
    Expr.CustomFunction(argExpr, "date_add", fn, captureResult)
  }

  final def dateDiff[V, T, U, P](
    lhsExpr: Expr[V, T, P],
    rhsExpr: Expr[V, T, P],
    roundToUnitExpr: Expr[V, U, P],
  )(implicit
    countTime: CountTime[T, U],
    argTypeInfo: Typeable[T :: T :: U :: HNil],
    captureArgResult: CaptureP[V, T :: T :: U :: HNil, P],
    captureResult: CaptureP[V, Long, P],
  ): Expr[V, Long, P] = {
    val argExpr = wrap(lhsExpr, rhsExpr, roundToUnitExpr).asHList.returnOutput
    val fn: T :: T :: U :: HNil => Long = {
      case lhs :: rhs :: unit :: HNil =>
        CountTime.between(lhs, rhs, unit)
    }
    Expr.CustomFunction(argExpr, "date_diff", fn, captureResult)
  }

  final def today[V, P](
    implicit
    captureClock: CaptureP[FactTable, Clock, P],
    captureEmbed: CaptureP[V, Clock, P],
    captureResult: CaptureP[V, LocalDate, P],
  ): Expr[V, LocalDate, P] =
    today(Clock.systemDefaultZone())

  final def today[V, P](
    clock: Clock,
  )(implicit
    captureClock: CaptureP[FactTable, Clock, P],
    captureEmbed: CaptureP[V, Clock, P],
    captureResult: CaptureP[V, LocalDate, P],
  ): Expr[V, LocalDate, P] =
    Expr.CustomFunction[V, Clock, LocalDate, P](embed(const(clock)), "today", LocalDate.now, captureResult)

  final def now[V, P](
    implicit
    captureClock: CaptureP[FactTable, Clock, P],
    captureEmbed: CaptureP[V, Clock, P],
    captureResult: CaptureP[V, Instant, P],
  ): Expr[V, Instant, P] =
    now(Clock.systemUTC())

  final def now[V, P](
    clock: Clock,
  )(implicit
    captureClock: CaptureP[FactTable, Clock, P],
    captureEmbed: CaptureP[V, Clock, P],
    captureResult: CaptureP[V, Instant, P],
  ): Expr[V, Instant, P] =
    Expr.CustomFunction[V, Clock, Instant, P](embed(const(clock)), "now", Instant.now, captureResult)

}
