package com.rallyhealth

import vapors.algebra.{CaptureP, Expr}
import vapors.data.{FactTable, TypedFact}
import vapors.dsl.{ExprBuilderCatsInstances, ExprBuilderSyntax, ExprDsl, WithOutputSyntax}

import shapeless.HList

package object vapors {

  // $COVERAGE-OFF$
  object core {

    @deprecated("Use com.rallyhealth.vapors.algebra instead.", "0.16.0")
    object algebra {

      @deprecated("Use com.rallyhealth.vapors.algebra.CaptureP instead.", "0.16.0")
      final type CaptureP[V, R, P] = vapors.algebra.CaptureP[V, R, P]

      @deprecated("Use com.rallyhealth.vapors.algebra.CaptureP instead.", "0.16.0")
      final val CaptureP = vapors.algebra.CaptureP

      @deprecated("Use com.rallyhealth.vapors.algebra.Expr instead.", "0.16.0")
      final type Expr[V, R, P] = vapors.algebra.Expr[V, R, P]

      @deprecated("Use com.rallyhealth.vapors.algebra.Expr instead.", "0.16.0")
      final val Expr = vapors.algebra.Expr

      @deprecated("Use com.rallyhealth.vapors.algebra.ConditionBranch instead.", "0.16.0")
      final type ConditionBranch[V, R, P] = vapors.algebra.ConditionBranch[V, R, P]

      @deprecated("Use com.rallyhealth.vapors.algebra.ConditionBranch instead.", "0.16.0")
      final val ConditionBranch = vapors.algebra.ConditionBranch

      @deprecated("Use com.rallyhealth.vapors.algebra.ExprConverter instead.", "0.16.0")
      final type ExprConverter[L, R] = vapors.algebra.ExprConverter[L, R]

      @deprecated("Use com.rallyhealth.vapors.algebra.ExprConverter instead.", "0.16.0")
      final val ExprConverter = vapors.algebra.ExprConverter

      @deprecated("Use com.rallyhealth.vapors.algebra.ExprResult instead.", "0.16.0")
      final type ExprResult[V, R, P] = vapors.algebra.ExprResult[V, R, P]

      @deprecated("Use com.rallyhealth.vapors.algebra.ExprResult instead.", "0.16.0")
      final val ExprResult = vapors.algebra.ExprResult

      @deprecated("Use com.rallyhealth.vapors.algebra.ExprSorter instead.", "0.16.0")
      final type ExprSorter[M[_], R] = vapors.algebra.ExprSorter[M, R]

      @deprecated("Use com.rallyhealth.vapors.algebra.ExprSorter instead.", "0.16.0")
      final val ExprSorter = vapors.algebra.ExprSorter

      @deprecated("Use com.rallyhealth.vapors.algebra.NonEmptyExprHList instead.", "0.16.0")
      final type NonEmptyExprHList[V, M[_], L <: HList, P] = vapors.algebra.NonEmptyExprHList[V, M, L, P]

      @deprecated("Use com.rallyhealth.vapors.algebra.NonEmptyExprHList instead.", "0.16.0")
      final val NonEmptyExprHList = vapors.algebra.NonEmptyExprHList
    }

    @deprecated("Use com.rallyhealth.vapors.data instead.", "0.16.0")
    object data {

      @deprecated("Use com.rallyhealth.vapors.data.Bounded instead.", "0.16.0")
      final type Bounded[A] = vapors.data.Bounded[A]

      @deprecated("Use com.rallyhealth.vapors.data.Bounded instead.", "0.16.0")
      final val Bounded = vapors.data.Bounded

      @deprecated("Use com.rallyhealth.vapors.data.Evidence instead.", "0.16.0")
      final type Evidence = vapors.data.Evidence

      @deprecated("Use com.rallyhealth.vapors.data.Evidence instead.", "0.16.0")
      final val Evidence = vapors.data.Evidence

      @deprecated("Use com.rallyhealth.vapors.data.ExtractBoolean instead.", "0.16.0")
      final type ExtractBoolean[-T] = vapors.data.ExtractValue[T, Boolean]

      @deprecated("Use com.rallyhealth.vapors.data.ExtractBoolean instead.", "0.16.0")
      final val ExtractBoolean = vapors.data.ExtractBoolean

      @deprecated("Use com.rallyhealth.vapors.data.ExtractInstant instead.", "0.16.0")
      final type ExtractInstant[-T] = vapors.data.ExtractInstant[T]

      @deprecated("Use com.rallyhealth.vapors.data.ExtractInstant instead.", "0.16.0")
      final val ExtractInstant = vapors.data.ExtractInstant

      @deprecated("Use com.rallyhealth.vapors.data.ExtractValue instead.", "0.16.0")
      final type ExtractValue[-T, +V] = vapors.data.ExtractValue[T, V]

      @deprecated("Use com.rallyhealth.vapors.data.ExtractValue instead.", "0.16.0")
      final val ExtractValue = vapors.data.ExtractValue

      @deprecated("Use com.rallyhealth.vapors.data.Fact instead.", "0.16.0")
      final type Fact = vapors.data.Fact

      @deprecated("Use com.rallyhealth.vapors.data.Fact instead.", "0.16.0")
      final val Fact = vapors.data.Fact

      @deprecated("Use com.rallyhealth.vapors.data.FactSet instead.", "0.16.0")
      final type FactSet = vapors.data.FactSet

      @deprecated("Use com.rallyhealth.vapors.data.FactSet instead.", "0.16.0")
      final val FactSet = vapors.data.FactSet

      @deprecated("Use com.rallyhealth.vapors.data.FactTable instead.", "0.16.0")
      final type FactTable = vapors.data.FactTable

      @deprecated("Use com.rallyhealth.vapors.data.FactTable instead.", "0.16.0")
      final val FactTable = vapors.data.FactTable

      @deprecated("Use com.rallyhealth.vapors.data.FactType instead.", "0.16.0")
      final type FactType[T] = vapors.data.FactType[T]

      @deprecated("Use com.rallyhealth.vapors.data.FactType instead.", "0.16.0")
      final val FactType = vapors.data.FactType

      @deprecated("Use com.rallyhealth.vapors.data.FactType instead.", "0.16.0")
      final type FactTypeSet[A] = vapors.data.FactTypeSet[A]

      @deprecated("Use com.rallyhealth.vapors.data.FactTypeSet instead.", "0.16.0")
      final val FactTypeSet = vapors.data.FactTypeSet

      @deprecated("Use com.rallyhealth.vapors.data.TimeOrder instead.", "0.16.0")
      final type TimeOrder = vapors.data.TimeOrder

      @deprecated("Use com.rallyhealth.vapors.data.TimeOrder instead.", "0.16.0")
      final val TimeOrder = vapors.data.TimeOrder

      @deprecated("Use com.rallyhealth.vapors.data.TypedFact instead.", "0.16.0")
      final type TypedFact[A] = vapors.data.TypedFact[A]

      @deprecated("Use com.rallyhealth.vapors.data.TypedFact instead.", "0.16.0")
      final val TypedFact = vapors.data.TypedFact

      @deprecated("Use com.rallyhealth.vapors.data.TypedFact instead.", "0.16.0")
      final type TypedFactSet[A] = vapors.data.TypedFactSet[A]

      @deprecated("Use com.rallyhealth.vapors.data.TypedFact instead.", "0.16.0")
      final val TypedFactSet = vapors.data.TypedFactSet

      @deprecated("Use com.rallyhealth.vapors.data.Window instead.", "0.16.0")
      final type Window[A] = vapors.data.Window[A]

      @deprecated("Use com.rallyhealth.vapors.data.Window instead.", "0.16.0")
      final val Window = vapors.data.Window
    }

    @deprecated("Use com.rallyhealth.vapors.dsl instead.", "0.16.0")
    object dsl extends ExprDsl with ExprBuilderSyntax with WithOutputSyntax with ExprBuilderCatsInstances {

      @deprecated("Use com.rallyhealth.vapors.dsl.CondExr instead.", "0.16.0")
      final type CondExpr[V, P] = Expr[V, Boolean, P]

      @deprecated("Use com.rallyhealth.vapors.dsl.ValExpr instead.", "0.16.0")
      final type ValExpr[V, R, P] = Expr[V, R, P]

      @deprecated("Use com.rallyhealth.vapors.dsl.ValCondExpr instead.", "0.16.0")
      final type ValCondExpr[V, P] = ValExpr[V, Boolean, P]

      @deprecated("Use com.rallyhealth.vapors.dsl.RootExpr instead.", "0.16.0")
      final type RootExpr[R, P] = Expr[FactTable, R, P]

      @deprecated("Use com.rallyhealth.vapors.dsl.CaptureRootExpr instead.", "0.16.0")
      final type CaptureRootExpr[R, P] = CaptureP[FactTable, R, P]

      @deprecated("Use com.rallyhealth.vapors.dsl.CaptureFromFacts instead.", "0.16.0")
      final type CaptureFromFacts[T, P] = CaptureP[Seq[TypedFact[T]], Seq[TypedFact[T]], P]

      @deprecated("Use com.rallyhealth.vapors.dsl.ConcatOutputExprBuilder instead.", "0.16.0")
      final type ConcatOutputExprBuilder[V, M[_], R, P] = vapors.dsl.ConcatOutputExprBuilder[V, M, R, P]

      @deprecated("Use com.rallyhealth.vapors.dsl.ConcatOutputExprBuilder instead.", "0.16.0")
      final val ConcatOutputExprBuilder = vapors.dsl.ConcatOutputExprBuilder

      @deprecated("Use com.rallyhealth.vapors.dsl.ExprBuilder instead.", "0.16.0")
      final type ExprBuilder[V, M[_], U, P] = vapors.dsl.ExprBuilder[V, M, U, P]

      @deprecated("Use com.rallyhealth.vapors.dsl.ExprBuilder instead.", "0.16.0")
      final val ExprBuilder = vapors.dsl.ExprBuilder

      @deprecated("Use com.rallyhealth.vapors.dsl.FoldableExprBuilder instead.", "0.16.0")
      final type FoldableExprBuilder[V, M[_], U, P] = vapors.dsl.FoldableExprBuilder[V, M, U, P]

      @deprecated("Use com.rallyhealth.vapors.dsl.ValExprBuilder instead.", "0.16.0")
      final type ValExprBuilder[V, R, P] = vapors.dsl.ValExprBuilder[V, R, P]

      @deprecated("Use com.rallyhealth.vapors.dsl.ExprDsl instead.", "0.16.0")
      final type ExprDsl = vapors.dsl.ExprDsl

      @deprecated("Use com.rallyhealth.vapors.dsl.ExprDsl instead.", "0.16.0")
      final val ExprDsl = vapors.dsl.ExprDsl
    }

    @deprecated("Use com.rallyhealth.vapors.interpreter instead.", "0.16.0")
    object interpreter {

      @deprecated("Use com.rallyhealth.vapors.interpreter.ExprInput instead.", "0.16.0")
      final type ExprInput[V] = vapors.interpreter.ExprInput[V]

      @deprecated("Use com.rallyhealth.vapors.interpreter.ExprInput instead.", "0.16.0")
      final val ExprInput = vapors.interpreter.ExprInput

      @deprecated("Use com.rallyhealth.vapors.interpreter.ExprOutput instead.", "0.16.0")
      final type ExprOutput[R] = vapors.interpreter.ExprOutput[R]

      @deprecated("Use com.rallyhealth.vapors.interpreter.ExprOutput instead.", "0.16.0")
      final val ExprOutput = vapors.interpreter.ExprOutput

      @deprecated("Use com.rallyhealth.vapors.interpreter.InterpretExprAsResultFn instead.", "0.16.0")
      final type InterpretExprAsResultFn[V, P] = vapors.interpreter.InterpretExprAsResultFn[V, P]

      @deprecated("Use com.rallyhealth.vapors.interpreter.InterpretExprAsResultFn instead.", "0.16.0")
      final val InterpretExprAsResultFn = vapors.interpreter.InterpretExprAsResultFn

      @deprecated("Use com.rallyhealth.vapors.interpreter.InterpretExprAsSimpleOutputFn instead.", "0.16.0")
      final type InterpretExprAsSimpleOutputFn[V, P] = vapors.interpreter.InterpretExprAsSimpleOutputFn[V, P]

      @deprecated("Use com.rallyhealth.vapors.interpreter.InterpretExprAsSimpleOutputFn instead.", "0.16.0")
      final val InterpretExprAsSimpleOutputFn = vapors.interpreter.InterpretExprAsSimpleOutputFn

      @deprecated("Use com.rallyhealth.vapors.interpreter.InterpretExprAsSimpleOutputFn instead.", "0.16.0")
      final type VisitGenericExprWithProxyFn[V, P, G[_]] = vapors.interpreter.VisitGenericExprWithProxyFn[V, P, G]
    }

    @deprecated("Use com.rallyhealth.vapors.lens instead.", "0.16.0")
    object lens {

      @deprecated("Use com.rallyhealth.vapors.lens.DataPath instead.", "0.16.0")
      final type DataPath = vapors.lens.DataPath

      @deprecated("Use com.rallyhealth.vapors.lens.DataPath instead.", "0.16.0")
      final val DataPath = vapors.lens.DataPath

      @deprecated("Use com.rallyhealth.vapors.lens.Indexed instead.", "0.16.0")
      final type Indexed[C, K, V] = vapors.lens.Indexed[C, K, V]

      @deprecated("Use com.rallyhealth.vapors.lens.Indexed instead.", "0.16.0")
      final val Indexed = vapors.lens.Indexed

      @deprecated("Use com.rallyhealth.vapors.lens.NamedLens instead.", "0.16.0")
      final type NamedLens[A, B] = vapors.lens.NamedLens[A, B]

      @deprecated("Use com.rallyhealth.vapors.lens.NamedLens instead.", "0.16.0")
      final val NamedLens = vapors.lens.NamedLens

      @deprecated("Use com.rallyhealth.vapors.lens.ValidDataPathKey instead.", "0.16.0")
      final type ValidDataPathKey[K] = vapors.lens.ValidDataPathKey[K]

      @deprecated("Use com.rallyhealth.vapors.lens.ValidDataPathKey instead.", "0.16.0")
      final val ValidDataPathKey = vapors.lens.ValidDataPathKey
    }

    @deprecated("Use com.rallyhealth.vapors.logic instead.", "0.16.0")
    object logic {

      @deprecated("Use com.rallyhealth.vapors.logic.Conjunction instead.", "0.16.0")
      final type Conjunction[A] = vapors.logic.Conjunction[A]

      @deprecated("Use com.rallyhealth.vapors.logic.Conjunction instead.", "0.16.0")
      final val Conjunction = vapors.logic.Conjunction

      @deprecated("Use com.rallyhealth.vapors.logic.Disjunction instead.", "0.16.0")
      final type Disjunction[A] = vapors.logic.Disjunction[A]

      @deprecated("Use com.rallyhealth.vapors.logic.Disjunction instead.", "0.16.0")
      final val Disjunction = vapors.logic.Disjunction

      @deprecated("Use com.rallyhealth.vapors.logic.Negation instead.", "0.16.0")
      final type Negation[A] = vapors.logic.Negation[A]

      @deprecated("Use com.rallyhealth.vapors.logic.Negation instead.", "0.16.0")
      final val Negation = vapors.logic.Negation
    }

    @deprecated("Use com.rallyhealth.vapors.math instead.", "0.16.0")
    object math {

      @deprecated("Use com.rallyhealth.vapors.math.Addition instead.", "0.16.0")
      final type Addition[A] = vapors.math.Addition[A]

      @deprecated("Use com.rallyhealth.vapors.math.Addition instead.", "0.16.0")
      final val Addition = vapors.math.Addition

      @deprecated("Use com.rallyhealth.vapors.math.Division instead.", "0.16.0")
      final type Division[A] = vapors.math.Division[A]

      @deprecated("Use com.rallyhealth.vapors.math.Division instead.", "0.16.0")
      final val Division = vapors.math.Division

      @deprecated("Use com.rallyhealth.vapors.math.Multiplication instead.", "0.16.0")
      final type Multiplication[A] = vapors.math.Multiplication[A]

      @deprecated("Use com.rallyhealth.vapors.math.Multiplication instead.", "0.16.0")
      final val Multiplication = vapors.math.Multiplication

      @deprecated("Use com.rallyhealth.vapors.math.Negative instead.", "0.16.0")
      final type Negative[A] = vapors.math.Negative[A]

      @deprecated("Use com.rallyhealth.vapors.math.Negative instead.", "0.16.0")
      final val Negative = vapors.math.Negative

      @deprecated("Use com.rallyhealth.vapors.math.Subtraction instead.", "0.16.0")
      final type Subtraction[A] = vapors.math.Subtraction[A]

      @deprecated("Use com.rallyhealth.vapors.math.Subtraction instead.", "0.16.0")
      final val Subtraction = vapors.math.Subtraction
    }

    @deprecated("Use com.rallyhealth.vapors.syntax instead.", "0.16.0")
    object syntax {

      @deprecated("Use com.rallyhealth.vapors.syntax.all instead.", "0.16.0")
      final val all = vapors.syntax.all

      @deprecated("Use com.rallyhealth.vapors.syntax.indexed instead.", "0.16.0")
      final val indexed = vapors.syntax.indexed

      @deprecated("Use com.rallyhealth.vapors.syntax.math instead.", "0.16.0")
      final val math = vapors.syntax.math
    }

    @deprecated("Use com.rallyhealth.vapors.time instead.", "0.16.0")
    object time {

      @deprecated("Use com.rallyhealth.vapors.time.CountTime instead.", "0.16.0")
      final type CountTime[-T, -U] = vapors.time.CountTime[T, U]

      @deprecated("Use com.rallyhealth.vapors.time.CountTime instead.", "0.16.0")
      final val CountTime = vapors.time.CountTime

      @deprecated("Use com.rallyhealth.vapors.time.ModifyTime instead.", "0.16.0")
      final type ModifyTime[T, -D] = vapors.time.ModifyTime[T, D]

      @deprecated("Use com.rallyhealth.vapors.time.ModifyTime instead.", "0.16.0")
      final val ModifyTime = vapors.time.ModifyTime
    }

    @deprecated("Use com.rallyhealth.vapors.util instead.", "0.16.0")
    object util {

      @deprecated("Use com.rallyhealth.vapors.util.ReflectUtils instead.", "0.16.0")
      final val ReflectUtils = vapors.util.ReflectUtils
    }
  }
  // $COVERAGE-ON$
}
