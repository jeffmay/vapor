package com.rallyhealth.vapors.factfilter.evaluator

import cats.{Foldable, FunctorFilter, Traverse, TraverseFilter}
import cats.kernel.Monoid
import com.rallyhealth.vapors.core.algebra.{Expr, ExprResult}
import com.rallyhealth.vapors.factfilter.data.Evidence
import com.rallyhealth.vapors.factfilter.data.circe._
import io.circe.syntax._
import io.circe.{Encoder, Json}
import shapeless.HList

object InterpretExprResultAsJson {

  type Out[_] = Json

  private final val instance = new InterpretExprResultAsJson[Any, Any, Any]
  private val exprEncoder: InterpretExprAsJson[Any, Any, Any] = new InterpretExprAsJson(encodeSubExpressions = false)

  def apply[F[_], V, R](): InterpretExprResultAsJson[F, V, R] =
    instance.asInstanceOf[InterpretExprResultAsJson[F, V, R]]

  def encode[F[_], V, R, P](op: ExprResult[F, V, R, P]): Json = op.visit(InterpretExprResultAsJson())

  /**
    * Encodes the given [[Expr]] using [[InterpretExprAsJson]], but without encoding the sub-expressions.
    */
  def encodeExpr[F[_], V, P](expr: Expr[F, V, _, P]): Json =
    expr.visit(exprEncoder.asInstanceOf[InterpretExprAsJson[F, V, P]])

  implicit def encodeEvidence: Encoder[Evidence] = encodeEvidenceWithToString

  implicit def encodeExprInput[F[_], V]: Encoder[ExprInput[F, V]] = { input =>
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

  // This takes a higher-kinded parameter because there isn't a good way to use a wild-card
  // See https://github.com/scala/bug/issues/8039 for more details
  protected def commonJsonEncoding[M[_], N[_]](
    expr: Expr[M, _, _, _],
    context: ExprResult.Context[N, _, _, _],
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
class InterpretExprResultAsJson[F[_], V, P] extends ExprResult.Visitor[F, V, P, InterpretExprResultAsJson.Out] {
  import InterpretExprResultAsJson._

  override def visitAddOutputs[R](result: ExprResult.AddOutputs[F, V, R, P]): Json =
    commonJsonEncoding(result.expr, result.context, skipOrEncodeMultiple(result.subResultList))

  override def visitAnd[R](result: ExprResult.And[F, V, R, P]): Json =
    commonJsonEncoding(result.expr, result.context, skipOrEncodeMultiple(result.subResultList))

  override def visitCollectFromOutput[M[_] : Foldable, U, R : Monoid](
    result: ExprResult.CollectFromOutput[F, V, M, U, R, P],
  ): Json =
    commonJsonEncoding(result.expr, result.context, skipOrEncodeInputResult(result.inputResult))

  override def visitConstOutput[R](result: ExprResult.ConstOutput[F, V, R, P]): Json =
    commonJsonEncoding(result.expr, result.context)

  override def visitDeclare[M[_], T](result: ExprResult.Define[F, V, M, T, P]): Json = {
    commonJsonEncoding(
      result.expr,
      result.context,
      skipOrEncodeSingle("definitionResult" -> result.definitionResult),
    )
  }

  override def visitEmbed[R](result: ExprResult.Embed[F, V, R, P]): Json = encode(result.embeddedResult)

  override def visitExistsInOutput[M[_] : Foldable, U](result: ExprResult.ExistsInOutput[F, V, M, U, P]): Json = {
    commonJsonEncoding(
      result.expr,
      result.context,
      skipOrEncodeInputResult(result.inputResult) ++
        skipOrEncodeMultiple("conditionResults" -> result.conditionResultList),
    )
  }

  override def visitFilterOutput[M[_] : Foldable : FunctorFilter, R](
    result: ExprResult.FilterOutput[F, V, M, R, P],
  ): Out[M[R]] = ???

  override def visitFlatMapOutput[M[_], U, R](result: ExprResult.FlatMapOutput[F, V, M, U, R, P]): Json = {
    commonJsonEncoding(
      result.expr,
      result.context,
      skipOrEncodeInputResult(result.inputResult) ++ skipOrEncodeMultiple(result.subResultList),
    )
  }

  override def visitMapOutput[M[_], U, R](result: ExprResult.MapOutput[F, V, M, U, R, P]): Json =
    commonJsonEncoding(result.expr, result.context)

  override def visitNegativeOutput[R](result: ExprResult.NegativeOutput[F, V, R, P]): Json =
    commonJsonEncoding(result.expr, result.context, skipOrEncodeSingle(result.inputResult))

  override def visitNot[R](result: ExprResult.Not[F, V, R, P]): Json =
    commonJsonEncoding(result.expr, result.context, skipOrEncodeSingle(result.subResult))

  override def visitOr[R](result: ExprResult.Or[F, V, R, P]): Json =
    commonJsonEncoding(result.expr, result.context, skipOrEncodeMultiple(result.subResultList))

  override def visitOutputIsEmpty[M[_] : Foldable, R](result: ExprResult.OutputIsEmpty[F, V, M, R, P]): Json =
    commonJsonEncoding(
      result.expr,
      result.context,
      skipOrEncodeInputResult(result.inputResult),
    )

  override def visitOutputWithinSet[R](result: ExprResult.OutputWithinSet[F, V, R, P]): Json =
    commonJsonEncoding(
      result.expr,
      result.context,
      skipOrEncodeInputResult(result.inputResult),
    )

  // TODO: Require a Show of R? Can this be done via post param if it took both R and P?
  override def visitOutputWithinWindow[R](result: ExprResult.OutputWithinWindow[F, V, R, P]): Json = {
    commonJsonEncoding(
      result.expr,
      result.context,
      skipOrEncodeInputResult(result.inputResult),
    )
  }

  override def visitReturnInput(result: ExprResult.ReturnInput[F, V, P]): Json =
    commonJsonEncoding(result.expr, result.context)

  override def visitSelectFromOutput[S, R](result: ExprResult.SelectFromOutput[F, V, S, R, P]): Json = {
    commonJsonEncoding(
      result.expr,
      result.context,
      skipOrEncodeInputResult(result.inputResult),
    )
  }

  override def visitSubtractOutputs[R](result: ExprResult.SubtractOutputs[F, V, R, P]): Json =
    commonJsonEncoding(result.expr, result.context, skipOrEncodeMultiple(result.subResultList))

  override def visitTakeFromOutput[M[_] : Traverse : TraverseFilter, R](
    result: ExprResult.TakeFromOutput[F, V, M, R, P],
  ): Json = {
    commonJsonEncoding(
      result.expr,
      result.context,
      skipOrEncodeInputResult(result.inputResult),
    )
  }

  override def visitUsingDefinitions[R](result: ExprResult.UsingDefinitions[F, V, R, P]): Json =
    commonJsonEncoding(result.expr, result.context)

  override def visitWhen[R](result: ExprResult.When[F, V, R, P]): Json = {
    def encodeUnevaluatedExpr(expr: Expr[F, V, R, P]): Json = {
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
      skipOrEncodeSingle(result.subResult) ++ Seq(
        "conditionBranches" -> (condBranchesJson :+ elseExprJson).asJson,
      ),
    )
  }

  override def visitWithFactsOfType[T, R](result: ExprResult.WithFactsOfType[F, V, T, R, P]): Json = {
    commonJsonEncoding(
      result.expr,
      result.context,
      skipOrEncodeSingle(result.subResult),
    )
  }

  override def visitWrapOutput[T <: HList, R](result: ExprResult.WrapOutput[F, V, T, R, P]): Json = {
    commonJsonEncoding(
      result.expr,
      result.context,
    )
  }

  // These take a higher-kinded parameter because there isn't a good way to use a wild-card
  // See https://github.com/scala/bug/issues/8039 for more details

  private final def skipOrEncodeInputResult[G[_]](result: ExprResult[G, _, _, _]): Seq[(String, Json)] =
    skipOrEncodeSingle("inputResult" -> result)

  private final def skipOrEncodeSingle[G[_]](result: ExprResult[G, _, _, _]): Seq[(String, Json)] =
    skipOrEncodeSingle("subResult" -> result)

  private final def skipOrEncodeSingle[G[_]](t: (String, ExprResult[G, _, _, _])): Seq[(String, Json)] = t match {
    case (name, result) =>
      if (result.isInstanceOf[ExprResult.ReturnInput[G, _, _]]) Nil
      else Seq(name -> encode(result))
  }

  private final def skipOrEncodeMultiple[G[_]](results: Seq[ExprResult[G, _, _, _]]): Seq[(String, Json)] =
    skipOrEncodeMultiple("subResultList" -> results)

  private final def skipOrEncodeMultiple[G[_]](t: (String, Seq[ExprResult[G, _, _, _]])): Seq[(String, Json)] =
    t match {
      case (name, results) =>
        val filteredResults = results.flatMap {
          case ExprResult.ReturnInput(_, _) => None
          case keep => Some(encode(keep))
        }
        if (filteredResults.isEmpty) Nil
        else Seq(name -> filteredResults.asJson)
    }
}
