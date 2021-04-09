package com.rallyhealth.vapors.circe.interpreter

import cats.{Align, Foldable, FunctorFilter, Monoid, MonoidK, Order, Traverse, TraverseFilter}
import com.rallyhealth.vapors.circe._
import com.rallyhealth.vapors.core.algebra.{Expr, ExprResult}
import com.rallyhealth.vapors.core.data.Evidence
import com.rallyhealth.vapors.core.interpreter.{ExprInput, ExprOutput}
import io.circe.syntax._
import io.circe.{Encoder, Json}
import shapeless.HList

import scala.collection.MapView

object InterpretExprResultAsJson {

  type Out[_] = Json

  private final val instance = new InterpretExprResultAsJson[Any, Any]
  private val exprEncoder: InterpretExprAsJson[Nothing, Nothing] = new InterpretExprAsJson(
    encodeSubExpressions = false,
  )

  def apply[V, R](): InterpretExprResultAsJson[V, R] =
    instance.asInstanceOf[InterpretExprResultAsJson[V, R]]

  def encode[V, R, P](op: ExprResult[V, R, P]): Json = op.visit(InterpretExprResultAsJson())

  /**
    * Encodes the given [[Expr]] using [[InterpretExprAsJson]], but without encoding the sub-expressions.
    */
  def encodeExpr[V, P](expr: Expr[V, _, P]): Json =
    expr.visit(exprEncoder.asInstanceOf[InterpretExprAsJson[V, P]])

  implicit def encodeEvidence: Encoder[Evidence] = encodeEvidenceWithToString

  implicit def encodeExprInput[V]: Encoder[ExprInput[V]] = { input =>
    Json.obj(
      "value" -> input.value.toString.asJson,
      "evidence" -> input.evidence.asJson,
    )
  }

  implicit def encodeExprOutput[R]: Encoder[ExprOutput[R]] = { output =>
    Json.obj(
      "value" -> output.value.toString.asJson,
      "evidence" -> output.evidence.asJson,
    )
  }

  protected def commonJsonEncoding(
    expr: Expr[_, _, _],
    context: ExprResult.Context[_, _, _],
    additionalFields: Seq[(String, Json)] = Nil,
  ): Json = {
    val obj = encodeExpr(expr)
    val result = {
      if (additionalFields.isEmpty) obj
      else Json.obj(additionalFields: _*).deepMerge(obj)
    }
    Json
      .obj(
        "input" -> context.input.asJson,
        "output" -> context.output.asJson,
      )
      .deepMerge(result)
      .asJson
  }

}

// TODO: Serialize param P
// TODO: Make it optional to stop encoding all subExpressions in the arguments list after a top-level expression?
//       Or maybe only print the input expression of each node (and arguments will encode all subExpressions)?
// TODO: Make it optional to unhide Embed node wrappers and just show their sub expression
// TODO: Make it optional to hide ReturnInput nodes
class InterpretExprResultAsJson[V, P] extends ExprResult.Visitor[V, P, InterpretExprResultAsJson.Out] {
  import InterpretExprResultAsJson._

  override def visitAddOutputs[R](result: ExprResult.AddOutputs[V, R, P]): Json =
    commonJsonEncoding(result.expr, result.context, skipOrEncodeSubResultList(result.subResultList))

  override def visitAnd[R](result: ExprResult.And[V, R, P]): Json =
    commonJsonEncoding(result.expr, result.context, skipOrEncodeSubResultList(result.subResultList))

  override def visitCollectFromOutput[M[_] : Foldable, U, R : Monoid](
    result: ExprResult.CollectFromOutput[V, M, U, R, P],
  ): Json =
    commonJsonEncoding(result.expr, result.context, skipOrEncodeInput(result.inputResult))

  override def visitConcatOutput[M[_] : MonoidK, R](result: ExprResult.ConcatOutput[V, M, R, P]): Out[M[R]] =
    commonJsonEncoding(result.expr, result.context, skipOrEncodeInputList(result.inputResultList))

  override def visitConstOutput[R](result: ExprResult.ConstOutput[V, R, P]): Json =
    commonJsonEncoding(result.expr, result.context)

  override def visitCustomFunction[A, R](result: ExprResult.CustomFunction[V, A, R, P]): Out[R] = ???

  override def visitDeclare[M[_], T](result: ExprResult.Define[V, M, T, P]): Json = {
    commonJsonEncoding(
      result.expr,
      result.context,
      skipOrEncodeSubResult("definitionResult", result.definitionResult),
    )
  }

  override def visitDivideOutputs[R](result: ExprResult.DivideOutputs[V, R, P]): Out[R] = ???

  override def visitEmbed[R](result: ExprResult.Embed[V, R, P]): Json = encode(result.embeddedResult)

  override def visitExistsInOutput[M[_] : Foldable, U](result: ExprResult.ExistsInOutput[V, M, U, P]): Json = {
    commonJsonEncoding(
      result.expr,
      result.context,
      skipOrEncodeInput(result.inputResult) ++
        skipOrEncodeSubResultList("conditionResults", result.conditionResultList),
    )
  }

  override def visitFilterOutput[M[_] : Foldable : FunctorFilter, R](
    result: ExprResult.FilterOutput[V, M, R, P],
  ): Out[M[R]] = {
    commonJsonEncoding(
      result.expr,
      result.context,
      skipOrEncodeInput(result.inputResult) ++ skipOrEncodeSubResultList(result.condResultList),
    )
  }

  override def visitFlatMapOutput[M[_], U, R](result: ExprResult.FlatMapOutput[V, M, U, R, P]): Json = {
    commonJsonEncoding(
      result.expr,
      result.context,
      skipOrEncodeInput(result.inputResult) ++ skipOrEncodeSubResultList(result.subResultList),
    )
  }

  override def visitFoldOutput[M[_] : Foldable, R : Monoid](result: ExprResult.FoldOutput[V, M, R, P]): Out[R] = ???

  override def visitGroupOutput[M[_] : Foldable, U : Order, K](
    result: ExprResult.GroupOutput[V, M, U, K, P],
  ): Out[MapView[K, Seq[U]]] =
    commonJsonEncoding(
      result.expr,
      result.context,
      skipOrEncodeInput(result.inputResult),
    )

  override def visitMapOutput[M[_], U, R](result: ExprResult.MapOutput[V, M, U, R, P]): Json =
    commonJsonEncoding(result.expr, result.context)

  override def visitMultiplyOutputs[R](result: ExprResult.MultiplyOutputs[V, R, P]): Out[R] = ???

  override def visitNegativeOutput[R](result: ExprResult.NegativeOutput[V, R, P]): Json =
    commonJsonEncoding(result.expr, result.context, skipOrEncodeSubResult(result.inputResult))

  override def visitNot[R](result: ExprResult.Not[V, R, P]): Json =
    commonJsonEncoding(result.expr, result.context, skipOrEncodeSubResult(result.subResult))

  override def visitOr[R](result: ExprResult.Or[V, R, P]): Json =
    commonJsonEncoding(result.expr, result.context, skipOrEncodeSubResultList(result.subResultList))

  override def visitOutputIsEmpty[M[_] : Foldable, R](result: ExprResult.OutputIsEmpty[V, M, R, P]): Json =
    commonJsonEncoding(
      result.expr,
      result.context,
      skipOrEncodeInput(result.inputResult),
    )

  override def visitOutputWithinSet[R](result: ExprResult.OutputWithinSet[V, R, P]): Json =
    commonJsonEncoding(
      result.expr,
      result.context,
      skipOrEncodeInput(result.inputResult),
    )

  // TODO: Require a Show of R? Can this be done via post param if it took both R and P?
  override def visitOutputWithinWindow[R](result: ExprResult.OutputWithinWindow[V, R, P]): Json = {
    commonJsonEncoding(
      result.expr,
      result.context,
      skipOrEncodeInput(result.inputResult),
    )
  }

  override def visitReturnInput(result: ExprResult.ReturnInput[V, P]): Json =
    commonJsonEncoding(result.expr, result.context)

  override def visitSelectFromOutput[S, R](result: ExprResult.SelectFromOutput[V, S, R, P]): Json = {
    commonJsonEncoding(
      result.expr,
      result.context,
      skipOrEncodeInput(result.inputResult),
    )
  }

  override def visitSortOutput[M[_], R](result: ExprResult.SortOutput[V, M, R, P]): Out[M[R]] = {
    commonJsonEncoding(result.expr, result.context, skipOrEncodeSubResult(result.inputResult))
  }

  override def visitSubtractOutputs[R](result: ExprResult.SubtractOutputs[V, R, P]): Json =
    commonJsonEncoding(result.expr, result.context, skipOrEncodeSubResultList(result.subResultList))

  override def visitTakeFromOutput[M[_] : Traverse : TraverseFilter, R](
    result: ExprResult.TakeFromOutput[V, M, R, P],
  ): Json = {
    commonJsonEncoding(
      result.expr,
      result.context,
      skipOrEncodeInput(result.inputResult),
    )
  }

  override def visitUsingDefinitions[R](result: ExprResult.UsingDefinitions[V, R, P]): Json =
    commonJsonEncoding(result.expr, result.context)

  override def visitWhen[R](result: ExprResult.When[V, R, P]): Json = {
    def encodeUnevaluatedExpr(expr: Expr[V, R, P]): Json = {
      val additionalFields = Json.obj(
        "output" -> Json.obj(
          "value" -> "(not evaluated)".asJson,
        ),
      )
      additionalFields.deepMerge(InterpretExprAsJson.encode(expr))
    }

    val condBranchesJson = result.expr.conditionBranches.toVector.map { branch =>
      Json.obj(
        "matched" -> result.matchedBranch.contains(branch).asJson,
        "whenExpr" -> encodeExpr(branch.whenExpr),
        "thenExpr" -> encodeUnevaluatedExpr(branch.thenExpr),
      )
    }
    val elseExprJson = Json.obj(
      "matched" -> result.matchedBranch.isEmpty.asJson,
      "elseExpr" -> encodeUnevaluatedExpr(result.expr.defaultExpr),
    )
    commonJsonEncoding(
      result.expr,
      result.context,
      skipOrEncodeSubResult(result.subResult) ++ Seq(
        "conditionBranches" -> (condBranchesJson :+ elseExprJson).asJson,
      ),
    )
  }

  override def visitWithFactsOfType[T, R](result: ExprResult.WithFactsOfType[V, T, R, P]): Json = {
    commonJsonEncoding(
      result.expr,
      result.context,
      skipOrEncodeSubResult(result.subResult),
    )
  }

  override def visitWrapOutputHList[T <: HList, R](result: ExprResult.WrapOutputHList[V, T, R, P]): Json = {
    commonJsonEncoding(
      result.expr,
      result.context,
    )
  }

  override def visitWrapOutputSeq[R](result: ExprResult.WrapOutputSeq[V, R, P]): Out[Seq[R]] = {
    commonJsonEncoding(
      result.expr,
      result.context,
    )
  }

  override def visitZipOutput[M[_] : Align : FunctorFilter, L <: HList, R](
    result: ExprResult.ZipOutput[V, M, L, R, P],
  ): Out[M[R]] = {
    commonJsonEncoding(
      result.expr,
      result.context,
    )
  }

  private final def skipOrEncodeInput(result: ExprResult[_, _, _]): Seq[(String, Json)] =
    skipOrEncodeInputList(Seq(result))

  private final def skipOrEncodeInputList(result: Seq[ExprResult[_, _, _]]): Seq[(String, Json)] =
    skipOrEncodeSubResultList("inputResults", result)

  private final def skipOrEncodeSubResult(result: ExprResult[_, _, _]): Seq[(String, Json)] =
    skipOrEncodeSubResultList(Seq(result))

  private final def skipOrEncodeSubResult(
    name: String,
    result: ExprResult[_, _, _],
  ): Seq[(String, Json)] =
    skipOrEncodeSubResultList(name, Seq(result))

  private final def skipOrEncodeSubResultList(results: Seq[ExprResult[_, _, _]]): Seq[(String, Json)] =
    skipOrEncodeSubResultList("subResults", results)

  private final def skipOrEncodeSubResultList(
    name: String,
    results: Seq[ExprResult[_, _, _]],
  ): Seq[(String, Json)] = {
    val filteredResults = results.flatMap {
      case ExprResult.ReturnInput(_, _) => None
      case keep => Some(encode(keep))
    }
    if (filteredResults.isEmpty) Nil
    else Seq(name -> filteredResults.asJson)
  }
}
