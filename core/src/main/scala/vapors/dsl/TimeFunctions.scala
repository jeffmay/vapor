package com.rallyhealth

package vapors.dsl

import vapors.algebra.{CaptureP, Expr}
import vapors.data.FactTable
import vapors.time.{CountTime, ModifyTime}

import shapeless._

import java.time.{Clock, Instant, LocalDate}

/**
  * Provides common functions work working with time.
  *
  * This works by providing an escape hatch for evaluating a custom Scala function (rather than building
  * all the operations out of [[Expr]] nodes).
  *
  * @see [[Expr.CustomFunction]]
  */
trait TimeFunctions {

  /**
    * Adds the result of the `rhs` temporal amount expression to the result of the `lhs` temporal expression.
    *
    * Requires a definition of [[ModifyTime]] for the two types. This is a safer API than working directly with
    * Java's Time library because it forbids adding [[java.time.Duration]]s to a [[LocalDate]] or adding a
    * [[java.time.Period]] to an [[Instant]]. Java Time will just throw an exception if the types are incompatible.
    *
    * @param lhsExpr an expression that computes a date and / or time
    * @param rhsExpr an expression that produces the duration of time to add (can be negative)
    *
    */
  // TODO: Can this be converted to Addition[T] if we make the second argument a different type? Maybe AddLeft[T, D]?
  final def dateAdd[V, T, D, P](
    lhsExpr: Expr[V, T, P],
    rhsExpr: Expr[V, D, P],
  )(implicit
    modTime: ModifyTime[T, D],
    argTypeInfo: Typeable[T :: D :: HNil],
    captureArgResult: CaptureP[V, T :: D :: HNil, P],
    captureResult: CaptureP[V, T, P],
  ): Expr[V, T, P] = {
    val argExpr = wrap(lhsExpr, rhsExpr).asHList.returnOutput
    val fn: T :: D :: HNil => T = {
      case lhs :: rhs :: HNil =>
        ModifyTime.addTo(lhs, rhs)
    }
    Expr.CustomFunction(argExpr, "date_add", fn, captureResult)
  }

  /**
    * Computes the difference between the `lhsExpr` and the `rhsExpr` as an integral value of the given unit
    * (can be negative). The result will be rounded down to the maximum number of whole units between the two
    * temporal instances that can be counted.
    *
    * This requires an instance of [[CountTime]] to make sure that the given unit type is safe to count for
    * the given temporal types. This is a safer API than working directly with Java's Time library because it
    * forbids subtracting two different temporal types from each other, without first converting them appropriately.
    * Java Time will just throw an exception if the types are incompatible.
    *
    * @param lhsExpr an expression that computes the temporal instant of when to start counting
    * @param rhsExpr an expression that computes the temporal instant of when to stop counting
    *                (either counting units of time in the past or future from lhsExpr)
    * @param roundToUnitExpr the type of unit to round down to
    */
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

  /**
    * Returns an expression that computes the current [[LocalDate]] when it is invoke, using the system
    * [[Clock]] with the default timezone information of host machine / JVM.
    */
  final def today[V, P](
    implicit
    captureClock: CaptureP[FactTable, Clock, P],
    captureEmbed: CaptureP[V, Clock, P],
    captureResult: CaptureP[V, LocalDate, P],
  ): Expr[V, LocalDate, P] =
    today(Clock.systemDefaultZone())

  /**
    * Returns an expression that computes the current [[LocalDate]] when it is invoked, based on the given [[Clock]].
    */
  final def today[V, P](
    clock: Clock,
  )(implicit
    captureClock: CaptureP[FactTable, Clock, P],
    captureEmbed: CaptureP[V, Clock, P],
    captureResult: CaptureP[V, LocalDate, P],
  ): Expr[V, LocalDate, P] =
    Expr.CustomFunction[V, Clock, LocalDate, P](embed(const(clock)), "today", LocalDate.now, captureResult)

  /**
    * Returns an expression that computes the current [[Instant]] when it is invoked, using the UTC [[Clock]].
    */
  final def now[V, P](
    implicit
    captureClock: CaptureP[FactTable, Clock, P],
    captureEmbed: CaptureP[V, Clock, P],
    captureResult: CaptureP[V, Instant, P],
  ): Expr[V, Instant, P] =
    now(Clock.systemUTC())

  /**
    * Returns an expression that computes the current [[Instant]] when it is invoked, based on the given [[Clock]].
    */
  final def now[V, P](
    clock: Clock,
  )(implicit
    captureClock: CaptureP[FactTable, Clock, P],
    captureEmbed: CaptureP[V, Clock, P],
    captureResult: CaptureP[V, Instant, P],
  ): Expr[V, Instant, P] =
    Expr.CustomFunction[V, Clock, Instant, P](embed(const(clock)), "now", Instant.now, captureResult)

}
