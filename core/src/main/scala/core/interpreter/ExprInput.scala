package com.rallyhealth.vapors

package core.interpreter

import core.data.{Evidence, FactTable}

final case class ExprInput[V](
  value: V,
  evidence: Evidence,
  factTable: FactTable,
) {

  @inline def withValue[U](
    value: U,
    evidence: Evidence = this.evidence,
  ): ExprInput[U] = copy(value = value, evidence = evidence)
}

object ExprInput {

  type Init = ExprInput[FactTable]

  @inline def fromFactTable(factTable: FactTable): Init =
    ExprInput(factTable, Evidence.none, factTable)

  @inline def fromValue[V](
    value: V,
    evidence: Evidence,
    factTable: FactTable,
  ): ExprInput[V] =
    ExprInput(value, evidence, factTable)

  @inline def fromValue[V](
    value: V,
    evidence: Evidence = Evidence.none,
  ): ExprInput[V] =
    fromValue(value, evidence, FactTable(evidence.factSet))
}
