package com.rallyhealth.vapors

package core.interpreter

import core.algebra.{ConditionBranch, Expr, ExprResult, NonEmptyExprHList}
import core.data._
import core.interpreter.InterpretExprAsSimpleOutputFn.SimpleOutputFnFunctorBuilder
import core.logic._
import core.math._

import cats._
import cats.data.Chain
import shapeless.HList

import scala.collection.MapView
import scala.collection.immutable.BitSet

final class InterpretExprAsResultFn[V, P] extends Expr.Visitor[V, P, Lambda[r => ExprInput[V] => ExprResult[V, r, P]]] {

  import cats.syntax.all._
  import com.rallyhealth.vapors.core.syntax.math._

  override def visitAddOutputs[R : Addition](expr: Expr.AddOutputs[V, R, P]): ExprInput[V] => ExprResult[V, R, P] = {
    input =>
      val inputResults = expr.inputExprList.map { inputExpr =>
        inputExpr.visit(this)(input)
      }
      val inputResultList = inputResults.toList
      val outputValue = inputResultList.map(_.output.value).reduceLeft(_ + _)
      val allEvidence = inputResultList.foldMap(_.output.evidence)
      val allParams = inputResultList.map(_.param)
      resultOfManySubExpr(expr, input, outputValue, allEvidence, allParams) {
        ExprResult.AddOutputs(_, _, inputResultList)
      }
  }

  override def visitAnd[R : Conjunction : ExtractBoolean](
    expr: Expr.And[V, R, P],
  ): ExprInput[V] => ExprResult[V, R, P] = { input =>
    val inputResults = expr.inputExprList.map { inputExpr =>
      inputExpr.visit(this)(input)
    }
    val combinedOutput = inputResults.map(_.output).reduceLeft(Conjunction[ExprOutput[R]].conjunction)
    val inputResultList = inputResults.toList
    val allParams = inputResultList.map(_.param)
    resultOfManySubExpr(expr, input, combinedOutput.value, combinedOutput.evidence, allParams) {
      ExprResult.And(_, _, inputResultList)
    }
  }

  override def visitCollectSomeOutput[M[_] : Foldable, U, R : Monoid](
    expr: Expr.CollectFromOutput[V, M, U, R, P],
  ): ExprInput[V] => ExprResult[V, R, P] = { input =>
    val inputResult = expr.inputExpr.visit(this)(input)
    val collectFn = expr.collectExpr.visit(InterpretExprAsResultFn())
    val (combinedResult, combinedEvidence, allParams) = inputResult.output.value.collectFoldSome { elem =>
      // If given a fact, use it as evidence, otherwise keep input evidence for this value of the collection
      val inputEvidence = Evidence.fromAny(elem).getOrElse(inputResult.output.evidence)
      val collectInput = input.withValue(elem, inputEvidence)
      val collectResult = collectFn(collectInput)
      // TODO: Keep all params / replay steps instead of just the ones that return true?
      collectResult.output.value.map { result =>
        (
          result,
          collectResult.output.evidence,
          collectResult.param :: Nil,
        )
      }
    }
    resultOfManySubExpr(expr, input, combinedResult, combinedEvidence, allParams) {
      ExprResult.CollectFromOutput(_, _, inputResult)
    }
  }

  override def visitConcatOutput[M[_] : MonoidK, R](
    expr: Expr.ConcatOutput[V, M, R, P],
  ): ExprInput[V] => ExprResult[V, M[R], P] = { input =>
    implicit val monoidMR: Monoid[M[R]] = MonoidK[M].algebra[R]
    val inputResults = expr.inputExprList.map(_.visit(this)(input))
    val allParams = inputResults.map(_.param).toList
    val allEvidence = inputResults.foldMap(_.output.evidence)
    val outputValues = inputResults.foldMap(_.output.value)
    resultOfManySubExpr(expr, input, outputValues, allEvidence, allParams) {
      ExprResult.ConcatOutput(_, _, inputResults)
    }
  }

  override def visitConstOutput[R](expr: Expr.ConstOutput[V, R, P]): ExprInput[V] => ExprResult[V, R, P] = { input =>
    resultOfPureExpr(expr, input, expr.value, input.evidence) {
      ExprResult.ConstOutput(_, _)
    }
  }

  override def visitCustomFunction[A, R](
    expr: Expr.CustomFunction[V, A, R, P],
  ): ExprInput[V] => ExprResult[V, R, P] = { input =>
    val argResult = InterpretExprAsResultFn(expr.inputExpr)(input)
    val value = expr.evaluate(argResult.output.value)
    resultOfPureExpr(expr, input, value, argResult.output.evidence) {
      ExprResult.CustomFunction(_, _, argResult)
    }
  }

  override def visitDefine[M[_] : Foldable, T](
    expr: Expr.Define[M, T, P],
  ): ExprInput[V] => ExprResult[V, FactSet, P] = { input =>
    val definitionFn = expr.definitionExpr.visit(InterpretExprAsResultFn())
    val definitionContext = input.withValue(input.factTable)
    val definitionResult = definitionFn(definitionContext)
    val definedFactSet = definitionResult.output.value.foldMap { definitionValue =>
      FactSet(DerivedFact(expr.factType, definitionValue, definitionResult.output.evidence))
    }
    val output = ExprOutput(definedFactSet, definitionResult.output.evidence)
    val postParam = expr.capture.foldToParam(expr, definitionContext, output, definitionResult.param :: Nil)
    ExprResult.Define(expr, ExprResult.Context(input, output, postParam), definitionResult)
  }

  override def visitEmbed[R](expr: Expr.Embed[V, R, P]): ExprInput[V] => ExprResult[V, R, P] = { input =>
    val embeddedFn = expr.embeddedExpr.visit(InterpretExprAsResultFn())
    val embeddedInput = input.withValue(input.factTable)
    val embeddedResult = embeddedFn(embeddedInput)
    // The evidence of the surrounding context is not relevant to the evidence of the embedded expression
    // so we ignore it and leave it up to the parent expression to combine evidence as it sees fit.
    resultOfSingleSubExpr(expr, input, embeddedResult) {
      ExprResult.Embed(_, _, embeddedResult)
    }
  }

  override def visitExistsInOutput[M[_] : Foldable, U](
    expr: Expr.ExistsInOutput[V, M, U, P],
  ): ExprInput[V] => ExprResult[V, Boolean, P] = { input =>
    val inputResult = expr.inputExpr.visit(this)(input)
    val conditionFn = expr.conditionExpr.visit(InterpretExprAsResultFn())
    val (allMatchedIndexes, allEvidence, allCondResults) = inputResult.output.value.toList.zipWithIndex.collectFold {
      case (elem, idx) =>
        // If given a fact, use it as evidence, otherwise keep input evidence for this value of the collection
        val inputEvidence = Evidence.fromAny(elem).getOrElse(inputResult.output.evidence)
        val conditionInput = input.withValue(elem, inputEvidence)
        val conditionResult = conditionFn(conditionInput)
        val isMatch = conditionResult.output.value
        val (matchedIdx, accEvidence, accResults) = {
          if (isMatch) (Chain(idx), conditionResult.output.evidence, Chain(conditionResult))
          else (Chain.nil, Evidence.none, Chain.nil)
        }
        (matchedIdx, accEvidence, accResults)
    }
    val matchedIndexSet = allMatchedIndexes.iterator.to(BitSet)
    val condResultList = allCondResults.toList
    val allParams = condResultList.map(_.param)
    // TODO: Better way to capture the param from the inputResult separate from the condResultList params?
    resultOfManySubExpr(expr, input, matchedIndexSet.nonEmpty, allEvidence, inputResult.param :: allParams) {
      ExprResult.ExistsInOutput(_, _, inputResult, condResultList, matchedIndexSet)
    }
  }

  override def visitFilterOutput[M[_] : Foldable : FunctorFilter, U](
    expr: Expr.FilterOutput[V, M, U, P],
  ): ExprInput[V] => ExprResult[V, M[U], P] = { input =>
    implicit val functorM: Functor[M] = FunctorFilter[M].functor
    val inputResult = expr.inputExpr.visit(this)(input)
    val condFn = expr.condExpr.visit(InterpretExprAsResultFn())
    val condResults = inputResult.output.value.map { elem =>
      val inputEvidence = Evidence.fromAny(elem).getOrElse(inputResult.output.evidence)
      val condInput = input.withValue(elem, inputEvidence)
      condFn(condInput)
    }
    val matchingValues = condResults.collect {
      case condResult if condResult.output.value => condResult.input.value
    }
    val (matchingEvidence, matchingParams) = condResults.collectFoldSome { result =>
      Option.when(result.output.value) {
        (result.output.evidence, result.param :: Nil)
      }
    }
    val condResultList = condResults.toList
    // TODO: Better way to capture the param from the inputResult separate from the condResultList params?
    resultOfManySubExpr(expr, input, matchingValues, matchingEvidence, inputResult.param :: matchingParams) {
      ExprResult.FilterOutput(_, _, inputResult, condResultList)
    }
  }

  override def visitFlatMapOutput[M[_] : Foldable : FlatMap, U, X](
    expr: Expr.FlatMapOutput[V, M, U, X, P],
  ): ExprInput[V] => ExprResult[V, M[X], P] = { input =>
    val inputResult = expr.inputExpr.visit(this)(input)
    val flatMapFn = expr.flatMapExpr.visit(InterpretExprAsResultFn())
    val allResults = inputResult.output.value.map { elem =>
      // If given a fact, use it as evidence, otherwise keep input evidence for this value of the collection
      val inputEvidence = Evidence.fromAny(elem).getOrElse(inputResult.output.evidence)
      val flatMapInput = input.withValue(elem, inputEvidence)
      flatMapFn(flatMapInput)
    }
    val allValues = allResults.flatMap(_.output.value)
    // TODO: Shouldn't I only look at the evidence of results that aren't empty?
    val (allEvidence, allParams) = allResults.foldMap { elemResult =>
      (elemResult.output.evidence, elemResult.param :: Nil)
    }
    val subOps = allResults.toList
    resultOfManySubExpr(expr, input, allValues, allEvidence, allParams) {
      ExprResult.FlatMapOutput(_, _, inputResult, subOps)
    }
  }

  override def visitGroupOutput[M[_] : Foldable, U : Order, K](
    expr: Expr.GroupOutput[V, M, U, K, P],
  ): ExprInput[V] => ExprResult[V, MapView[K, Seq[U]], P] = { input =>
    val inputResult = expr.inputExpr.visit(this)(input)
    val values = inputResult.output.value
    val grouped = values.toIterable.groupBy(expr.groupByLens.get).view.mapValues(_.to(LazyList))
    resultOfManySubExpr(expr, input, grouped, inputResult.output.evidence, inputResult.param :: Nil) {
      ExprResult.GroupOutput(_, _, inputResult)
    }
  }

  override def visitMapOutput[M[_] : Foldable : Functor, U, R](
    expr: Expr.MapOutput[V, M, U, R, P],
  ): ExprInput[V] => ExprResult[V, M[R], P] = { input =>
    val inputResult = expr.inputExpr.visit(this)(input)
    val mapFn = expr.mapExpr.visit(InterpretExprAsResultFn())
    val allResults = inputResult.output.value.map { elem =>
      // If given a fact, use it as evidence, otherwise keep input evidence for this value of the collection
      val inputEvidence = Evidence.fromAny(elem).getOrElse(inputResult.output.evidence)
      val mapInput = input.withValue(elem, inputEvidence)
      mapFn(mapInput)
    }
    val allValues = allResults.map(_.output.value)
    val (allEvidence, allParams) = allResults.foldMap { elemResult =>
      (elemResult.output.evidence, elemResult.param :: Nil)
    }
    val subOps = allResults.toList
    resultOfManySubExpr(expr, input, allValues, allEvidence, allParams) {
      ExprResult.MapOutput(_, _, inputResult, subOps)
    }
  }

  override def visitMultiplyOutputs[R : Multiplication](
    expr: Expr.MultiplyOutputs[V, R, P],
  ): ExprInput[V] => ExprResult[V, R, P] = { input =>
    val inputResults = expr.inputExprList.map { inputExpr =>
      inputExpr.visit(this)(input)
    }
    val inputResultList = inputResults.toList
    val outputValue = inputResultList.map(_.output.value).reduceLeft(_ * _)
    val allEvidence = inputResultList.foldMap(_.output.evidence)
    val allParams = inputResultList.map(_.param)
    resultOfManySubExpr(expr, input, outputValue, allEvidence, allParams) {
      ExprResult.MultiplyOutputs(_, _, inputResultList)
    }
  }

  override def visitNegativeOutput[R : Negative](
    expr: Expr.NegativeOutput[V, R, P],
  ): ExprInput[V] => ExprResult[V, R, P] = { input =>
    val inputResult = expr.inputExpr.visit(this)(input)
    val outputValue = Negative[R].negative(inputResult.output.value)
    resultOfManySubExpr(expr, input, outputValue, inputResult.output.evidence, inputResult.param :: Nil) {
      ExprResult.NegativeOutput(_, _, inputResult)
    }
  }

  override def visitNot[R : Negation](expr: Expr.Not[V, R, P]): ExprInput[V] => ExprResult[V, R, P] = { input =>
    val inputResult = expr.inputExpr.visit(this)(input)
    val outputValue = Negation[R].negation(inputResult.output.value)
    resultOfManySubExpr(expr, input, outputValue, inputResult.output.evidence, inputResult.param :: Nil) {
      ExprResult.Not(_, _, inputResult)
    }
  }

  override def visitOr[R : Disjunction : ExtractBoolean](
    expr: Expr.Or[V, R, P],
  ): ExprInput[V] => ExprResult[V, R, P] = { input =>
    val inputResults = expr.inputExprList.map { inputExpr =>
      inputExpr.visit(this)(input)
    }
    val combinedOutput = inputResults.map(_.output).reduceLeft(Disjunction[ExprOutput[R]].disjunction)
    val inputResultList = inputResults.toList
    val allParams = inputResultList.map(_.param)
    resultOfManySubExpr(expr, input, combinedOutput.value, combinedOutput.evidence, allParams) {
      ExprResult.Or(_, _, inputResultList)
    }
  }

  override def visitOutputIsEmpty[M[_] : Foldable, R](
    expr: Expr.OutputIsEmpty[V, M, R, P],
  ): ExprInput[V] => ExprResult[V, Boolean, P] = { input =>
    val inputResult = expr.inputExpr.visit(this)(input)
    val isEmpty = inputResult.output.value.isEmpty
    resultOfPureExpr(expr, input, isEmpty, inputResult.output.evidence) {
      ExprResult.OutputIsEmpty(_, _, inputResult)
    }
  }

  override def visitOutputWithinSet[R](
    expr: Expr.OutputWithinSet[V, R, P],
  ): ExprInput[V] => ExprResult[V, Boolean, P] = { input =>
    val inputResult = expr.inputExpr.visit(this)(input)
    val isWithinSet = expr.accepted.contains(inputResult.output.value)
    resultOfPureExpr(expr, input, isWithinSet, inputResult.output.evidence) {
      ExprResult.OutputWithinSet(_, _, inputResult)
    }
  }

  override def visitOutputWithinWindow[R](
    expr: Expr.OutputWithinWindow[V, R, P],
  ): ExprInput[V] => ExprResult[V, Boolean, P] = { input =>
    val inputResult = expr.inputExpr.visit(this)(input)
    val isWithinWindow = expr.window.contains(inputResult.output.value)
    resultOfPureExpr(expr, input, isWithinWindow, inputResult.output.evidence) {
      ExprResult.OutputWithinWindow(_, _, inputResult)
    }
  }

  override def visitFoldOutput[M[_] : Foldable, R : Monoid](
    expr: Expr.FoldOutput[V, M, R, P],
  ): ExprInput[V] => ExprResult[V, R, P] = { input =>
    val inputResult = expr.inputExpr.visit(this)(input)
    val resultValue = inputResult.output.value.fold
    resultOfPureExpr(expr, input, resultValue, inputResult.output.evidence) {
      ExprResult.FoldOutput(_, _, inputResult)
    }
  }

  override def visitReturnInput(expr: Expr.ReturnInput[V, P]): ExprInput[V] => ExprResult[V, V, P] = { input =>
    resultOfPureExpr(expr, input, input.value, input.evidence ++ Evidence.fromAnyOrNone(input.value)) {
      ExprResult.ReturnInput(_, _)
    }
  }

  override def visitSelectFromOutput[S, R](
    expr: Expr.SelectFromOutput[V, S, R, P],
  ): ExprInput[V] => ExprResult[V, R, P] = { input =>
    val inputResult = expr.inputExpr.visit(this)(input)
    val selected = expr.lens.get(inputResult.output.value)
    resultOfManySubExpr(expr, input, selected, inputResult.output.evidence, inputResult.param :: Nil) {
      ExprResult.SelectFromOutput(_, _, inputResult)
    }
  }

  override def visitSortOutput[M[_], R](expr: Expr.SortOutput[V, M, R, P]): ExprInput[V] => ExprResult[V, M[R], P] = {
    input =>
      val inputResult = expr.inputExpr.visit(this)(input)
      val unsorted = inputResult.output.value
      val sorted = expr.sorter(unsorted)
      resultOfManySubExpr(expr, input, sorted, inputResult.output.evidence, inputResult.param :: Nil) {
        ExprResult.SortOutput(_, _, inputResult)
      }
  }

  override def visitSubtractOutputs[R : Subtraction](
    expr: Expr.SubtractOutputs[V, R, P],
  ): ExprInput[V] => ExprResult[V, R, P] = { input =>
    val allResults = expr.inputExprList.map(_.visit(this)(input))
    val allResultsList = allResults.toList
    val addResult = allResultsList.map(_.output.value).reduceLeft(_ - _)
    val allEvidence = allResultsList.foldMap(_.output.evidence)
    val allParams = allResultsList.map(_.param)
    resultOfManySubExpr(expr, input, addResult, allEvidence, allParams) {
      ExprResult.SubtractOutputs(_, _, allResultsList)
    }
  }

  override def visitDivideOutputs[R : Division](
    expr: Expr.DivideOutputs[V, R, P],
  ): ExprInput[V] => ExprResult[V, R, P] = { input =>
    val allResults = expr.inputExprList.map(_.visit(this)(input))
    val allResultsList = allResults.toList
    val addResult = allResultsList.map(_.output.value).reduceLeft(_ / _)
    val allEvidence = allResultsList.foldMap(_.output.evidence)
    val allParams = allResultsList.map(_.param)
    resultOfManySubExpr(expr, input, addResult, allEvidence, allParams) {
      ExprResult.DivideOutputs(_, _, allResultsList)
    }
  }

  override def visitTakeFromOutput[M[_] : Traverse : TraverseFilter, R](
    expr: Expr.TakeFromOutput[V, M, R, P],
  ): ExprInput[V] => ExprResult[V, M[R], P] = { input =>
    val inputResult = expr.inputExpr.visit(this)(input)
    val values = inputResult.output.value
    val takeWindow: Window[Int] = expr.take match {
      case pos if pos > 0 => Window.between(0, pos)
      case 0 => Window.empty
      case neg if neg < 0 =>
        val totalSize = values.size.toInt
        Window.between(totalSize + neg, totalSize)
    }
    // TODO: Handle evidence when the input is a set / sequence of facts
    val selectedValues = values.zipWithIndex.collect {
      case (elem, idx) if takeWindow.contains(idx) => elem
    }
    resultOfManySubExpr(expr, input, selectedValues, inputResult.output.evidence, inputResult.param :: Nil) {
      ExprResult.TakeFromOutput(_, _, inputResult)
    }
  }

  override def visitWrapOutputSeq[R](expr: Expr.WrapOutputSeq[V, R, P]): ExprInput[V] => ExprResult[V, Seq[R], P] = {
    input =>
      val inputResultList = expr.inputExprList.to(LazyList).map(_.visit(this)(input))
      val allEvidence = inputResultList.foldMap(_.output.evidence)
      val allParams = inputResultList.foldMap(_.param :: Nil)
      val outputValues = inputResultList.map(_.output.value)
      resultOfManySubExpr(expr, input, outputValues, allEvidence, allParams) {
        ExprResult.WrapOutputSeq(_, _, inputResultList)
      }
  }

  override def visitWrapOutputHList[T <: HList, R](
    expr: Expr.WrapOutputHList[V, T, R, P],
  ): ExprInput[V] => ExprResult[V, R, P] = { input =>
    val (tupleOutput, allParams) = visitHListOfScalarExprAndCombineOutput(expr.inputExprHList, input)
    val value = expr.converter(tupleOutput.value)
    resultOfManySubExpr(expr, input, value, tupleOutput.evidence, allParams) {
      ExprResult.WrapOutputHList(_, _)
    }
  }

  override def visitUsingDefinitions[R](expr: Expr.UsingDefinitions[V, R, P]): ExprInput[V] => ExprResult[V, R, P] = {
    input =>
      val definitionVisitor = new InterpretExprAsResultFn[FactTable, P]
      val definitionInput = input.withValue(input.factTable)
      val (declaredFacts, evidence, declaredParams) = expr.definitions.foldMap { defExpr =>
        val definitionFn = defExpr.visit(definitionVisitor)
        val definitionResult = definitionFn(definitionInput)
        (definitionResult.output.value, definitionResult.output.evidence, definitionResult.param :: Nil)
      }
      val subInput = input.copy(factTable = input.factTable.addAll(declaredFacts))
      val subFn = expr.subExpr.visit(this)
      val subResult = subFn(subInput)
      // TODO: Come up with a better way to combine the CaptureP params from expressions that have multiple
      //       sub expressions with different meanings.
      val allParams = subResult.param :: declaredParams
      val postParam = expr.capture.foldToParam(expr, input, subResult.output, allParams)
      ExprResult.UsingDefinitions(expr, ExprResult.Context(input, subResult.output, postParam), subResult)
  }

  override def visitWhen[R](expr: Expr.When[V, R, P]): ExprInput[V] => ExprResult[V, R, P] = { input =>
    val (maybeConditionResult, condParams) = {
      expr.conditionBranches.foldLeft((None, Nil): (Option[(ConditionBranch[V, R, P], Evidence)], List[Eval[P]])) {
        case (acc @ (Some(_), _), _) => acc
        case ((None, params), cond) =>
          val whenResult = cond.whenExpr.visit(this)(input)
          val conditionMet = whenResult.output.value
          (Option.when(conditionMet)((cond, whenResult.output.evidence)), whenResult.param :: params)
      }
    }
    val (thenExpr, condEvidence) = maybeConditionResult
      .map {
        case (branch, evidence) => (branch.thenExpr, evidence)
      }
      .getOrElse {
        (expr.defaultExpr, Evidence.none)
      }
    val thenResult = thenExpr.visit(this)(input)
    // union the evidence for the condition with the evidence for the output
    val allEvidence = condEvidence.union(thenResult.output.evidence)
    // TODO: Better way to organize the params than to just put the thenExpr param at the head?
    val allParams = thenResult.param :: condParams
    resultOfManySubExpr(expr, input, thenResult.output.value, allEvidence, allParams) {
      ExprResult.When(_, _, maybeConditionResult.map(_._1), thenResult)
    }
  }

  override def visitWithFactsOfType[T, R](
    expr: Expr.WithFactsOfType[T, R, P],
  ): ExprInput[V] => ExprResult[V, R, P] = { input =>
    val inputFactTable = input.withValue(input.factTable)
    val withMatchingFactsFn = expr.subExpr.visit(InterpretExprAsResultFn())
    val matchingFacts = input.factTable.getSortedSeq(expr.factTypeSet)
    // facts will always be added as their own evidence when used, so we do not need to add them to the evidence here
    val subInput = input.withValue[Seq[TypedFact[T]]](matchingFacts)
    val subResult = withMatchingFactsFn(subInput)
    val postParam = expr.capture.foldToParam(expr, inputFactTable, subResult.output, subResult.param :: Nil)
    ExprResult.WithFactsOfType(
      expr,
      ExprResult.Context(input, subResult.output, postParam),
      subResult,
    )
  }

  override def visitZipOutput[M[_] : Align : FunctorFilter, L <: HList, R](
    expr: Expr.ZipOutput[V, M, L, R, P],
  ): ExprInput[V] => ExprResult[V, M[R], P] = { input =>
    val (tupleOutput, allParams) =
      visitHListOfHigherKindedExprAndZipCombinedToShortestOutput(expr.inputExprHList, input)
    val outputValues = FunctorFilter[M].functor.map(tupleOutput.value)(expr.converter)
    resultOfManySubExpr(expr, input, outputValues, tupleOutput.evidence, allParams) {
      ExprResult.ZipOutput(_, _)
    }
  }

  @inline private def resultOfPureExpr[R](
    expr: Expr[V, R, P],
    input: ExprInput[V],
    result: R,
    evidence: Evidence,
  )(
    buildPostOp: (expr.type, ExprResult.Context[V, R, P]) => ExprResult[V, R, P],
  ): ExprResult[V, R, P] = {
    resultOfManySubExpr(expr, input, result, evidence, Nil)(buildPostOp)
  }

  @inline private def resultOfManySubExpr[R](
    expr: Expr[V, R, P],
    input: ExprInput[V],
    result: R,
    evidence: Evidence,
    capturedParams: List[Eval[P]],
  )(
    buildResult: (expr.type, ExprResult.Context[V, R, P]) => ExprResult[V, R, P],
  ): ExprResult[V, R, P] = {
    val output = ExprOutput(result, evidence)
    val param = expr.capture.foldToParam(expr, input, output, capturedParams)
    buildResult(expr, ExprResult.Context(input, output, param))
  }

  @inline private def resultOfSingleSubExpr[R](
    expr: Expr[V, R, P],
    input: ExprInput[V],
    subResult: ExprResult[_, R, P],
  )(
    buildPostOp: (expr.type, ExprResult.Context[V, R, P]) => ExprResult[V, R, P],
  ): ExprResult[V, R, P] = {
    resultOfManySubExpr(expr, input, subResult.output.value, subResult.output.evidence, subResult.param :: Nil) {
      buildPostOp
    }
  }

  /**
    * This type and functor are used for HList operations that require the [[InterpretExprAsSimpleOutputFn]] interpreter
    */
  private object SimpleOutputFnFunctorBuilder extends SimpleOutputFnFunctorBuilder[V, P]
  import SimpleOutputFnFunctorBuilder._

  /**
    * Uses the [[SimpleOutputFunctor]] to visit and map over each expression of the given `exprHList` and return
    * a simplified output of [[InterpretExprAsSimpleOutputFn.GenSimpleOutput]] combined as a Monoid.
    *
    * The evidence sets are unioned together, the params are combined in a list, and the values are mapped into
    * an [[HList]] of the given type.
    *
    * @see [[NonEmptyExprHList.visitProduct]] for more details and examples
    */
  private def visitHListOfScalarExprAndCombineOutput[L <: HList](
    exprHList: NonEmptyExprHList[V, Id, L, P],
    input: ExprInput[V],
  ): SimpleOutput[L] = {
    exprHList.visitProduct(new InterpretExprAsSimpleOutputFn).apply(input)
  }

  /**
    * Similar to [[visitHListOfScalarExprAndCombineOutput]] except that it handles an HList of expressions that return
    * higher-kinded functor wrapped values by aligning and zipping the values as HList values of each element
    * inside the wrapper.
    *
    * In other words, it only returns a wrapped HList for each element of the shortest-length output value.
    *
    * @see [[NonEmptyExprHList.visitZippedToShortest]] for more details and examples.
    */
  private def visitHListOfHigherKindedExprAndZipCombinedToShortestOutput[M[_] : Align : FunctorFilter, L <: HList](
    exprHList: NonEmptyExprHList[V, M, L, P],
    input: ExprInput[V],
  ): SimpleOutput[M[L]] = {
    exprHList.visitZippedToShortest(new InterpretExprAsSimpleOutputFn).apply(input)
  }
}

object InterpretExprAsResultFn {

  final def apply[V, P](): InterpretExprAsResultFn[V, P] = new InterpretExprAsResultFn

  final def apply[V, R, P](expr: Expr[V, R, P])(input: ExprInput[V]): ExprResult[V, R, P] = {
    expr.visit(InterpretExprAsResultFn())(input)
  }
}
