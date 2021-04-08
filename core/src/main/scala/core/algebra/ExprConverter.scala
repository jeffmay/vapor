package com.rallyhealth.vapors

package core.algebra

import shapeless.ops.hlist.Tupler
import shapeless.{Generic, HList}

sealed trait ExprConverter[L, R] extends (L => R) {
  def conversionType: String
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
}
