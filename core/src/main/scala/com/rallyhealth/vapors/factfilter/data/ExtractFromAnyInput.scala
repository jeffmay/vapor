package com.rallyhealth.vapors.factfilter.data

import com.rallyhealth.vapors.factfilter.evaluator.InterpretExprAsFunction.Input

trait ExtractFromAnyInput[U] {

  def extractFromInput[F[_], V](input: Input[F, V]): U
}

object ExtractFromAnyInput {

  implicit val extractUnitFromInput: ExtractFromAnyInput[Unit] = new ExtractFromAnyInput[Unit] {
    override final def extractFromInput[F[_], V](input: Input[F, V]): Unit = ()
  }

  implicit val extractFactTableFromInput: ExtractFromAnyInput[FactTable] = new ExtractFromAnyInput[FactTable] {
    override final def extractFromInput[F[_], V](input: Input[F, V]): FactTable = input.factTable
  }
}
