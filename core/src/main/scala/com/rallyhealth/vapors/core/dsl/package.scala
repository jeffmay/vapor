package com.rallyhealth.vapors.core

import com.rallyhealth.vapors.core.algebra.{CaptureP, Expr}
import com.rallyhealth.vapors.core.data.{FactTable, TypedFact}

package object dsl extends ExprDsl with ExprBuilderSyntax with WithOutputSyntax with ExprBuilderCatsInstances {

  final type CondExpr[V, P] = Expr[V, Boolean, P]

  final type ValExpr[V, R, P] = Expr[V, R, P]
  final type ValCondExpr[V, P] = ValExpr[V, Boolean, P]

  final type RootExpr[R, P] = Expr[FactTable, R, P]

  final type CaptureRootExpr[R, P] = CaptureP[FactTable, R, P]
  final type CaptureFromFacts[T, P] = CaptureP[Seq[TypedFact[T]], Seq[TypedFact[T]], P]
}
