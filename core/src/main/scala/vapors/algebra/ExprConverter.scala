package com.rallyhealth

package vapors.algebra

import vapors.data.Window

import cats.Order
import shapeless.ops.hlist.Tupler
import shapeless.{Generic, HList}

/**
  * A serializable description of a total conversion function from one type to another.
  */
sealed trait ExprConverter[L, R] extends (L => R) {

  /**
    * The type of conversion applied.
    */
  def conversionType: String

  /**
    * Apply the conversion.
    *
    * @note this should never throw an exception.
    */
  override def apply(inputValue: L): R
}

object ExprConverter {

  // TODO: Capture type information about R for debugging?
  private final class Impl[L, R](
    override val conversionType: String,
    convert: L => R,
  ) extends ExprConverter[L, R] {
    override def apply(in: L): R = convert(in)
    override val toString: String = s"ExprConverter.$conversionType"
  }

  def asHListIdentity[R <: HList]: ExprConverter[R, R] = new Impl("asHList", identity)

  def asProductType[L <: HList, R](implicit gen: Generic.Aux[R, L]): ExprConverter[L, R] =
    new Impl("asProduct", gen.from)

  def asTuple[L <: HList, R](implicit tupler: Tupler.Aux[L, R]): ExprConverter[L, R] =
    new Impl("asTuple", tupler.apply)

  def asWindow[R : Order](buildWindow: R => Window[R]): ExprConverter[R, Window[R]] =
    new Impl("asWindow", buildWindow)
}
