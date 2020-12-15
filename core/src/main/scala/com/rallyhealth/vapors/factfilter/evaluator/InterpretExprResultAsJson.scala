package com.rallyhealth.vapors.factfilter.evaluator

import cats.Foldable
import cats.kernel.Monoid
import com.rallyhealth.vapors.core.algebra.{Expr, ExprResult}
import com.rallyhealth.vapors.factfilter.data.ResultSet
import com.rallyhealth.vapors.factfilter.data.circe._
import io.circe.syntax._
import io.circe.{Encoder, Json}

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

  // This takes a higher-kinded parameter because there isn't a good way to use a wild-card
  // See https://github.com/scala/bug/issues/8039 for more details
  protected def commonJsonEncoding[M[_], N[_]](
    expr: Expr[M, _, _, _],
    context: ExprResult.Context[N, _, _, _],
    additionalFields: Seq[(String, Json)] = Nil,
  ): Json = {
    implicit def encodeEvidence: Encoder[ResultSet] = encodeEvidenceWithToString
    val obj = encodeExpr(expr)
    val result = {
      if (additionalFields.isEmpty) obj
      else Json.obj(additionalFields: _*).deepMerge(obj)
    }
    Json
      .obj(
        "input" -> Json.obj(
          "value" -> context.input.value.toString.asJson,
          "evidence" -> context.input.evidence.asJson,
        ),
        "output" -> Json.obj(
          "value" -> context.output.value.toString.asJson,
          "evidence" -> context.output.evidence.asJson,
        ),
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
    commonJsonEncoding(result.expr, result.context, skipOrEncodeinputResult(result.inputResult))

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
      skipOrEncodeinputResult(result.inputResult) ++
        skipOrEncodeMultiple("conditionResults" -> result.conditionResultList),
    )
  }

  override def visitFlatMapOutput[M[_], U, R](result: ExprResult.FlatMapOutput[F, V, M, U, R, P]): Json = {
    commonJsonEncoding(
      result.expr,
      result.context,
      skipOrEncodeinputResult(result.inputResult) ++ skipOrEncodeMultiple(result.subResultList),
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

  // TODO: Require a Show of R? Can this be done via post param if it took both R and P?
  override def visitOutputWithinWindow[R](result: ExprResult.OutputWithinWindow[F, V, R, P]): Json = {
    commonJsonEncoding(
      result.expr,
      result.context,
      skipOrEncodeinputResult(result.inputResult),
    )
  }

  override def visitReturnInput(result: ExprResult.ReturnInput[F, V, P]): Json =
    commonJsonEncoding(result.expr, result.context)

  override def visitSelectFromOutput[S, R](result: ExprResult.SelectFromOutput[F, V, S, R, P]): Json = {
    commonJsonEncoding(
      result.expr,
      result.context,
      skipOrEncodeinputResult(result.inputResult),
    )
  }

  override def visitSubtractOutputs[R](result: ExprResult.SubtractOutputs[F, V, R, P]): Json =
    commonJsonEncoding(result.expr, result.context, skipOrEncodeMultiple(result.subResultList))

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

    def thenExprJson = encodeUnevaluatedExpr(result.expr.thenExpr)
    def elseExprJson = encodeUnevaluatedExpr(result.expr.elseExpr)
    val condResultJson = encode(result.conditionResult)
    val subResultJson = encode(result.subResult)
    val (thenDebug, elseDebug) = if (result.conditionMet) {
      ("thenResult" -> condResultJson, "elseResult" -> elseExprJson)
    } else {
      ("thenResult" -> thenExprJson, "elseResult" -> subResultJson)
    }
    commonJsonEncoding(
      result.expr,
      result.context,
      skipOrEncodeSingle("conditionResult" -> result.conditionResult) ++ Seq(
        thenDebug,
        elseDebug,
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

  // These take a higher-kinded parameter because there isn't a good way to use a wild-card
  // See https://github.com/scala/bug/issues/8039 for more details

  private final def skipOrEncodeinputResult[G[_]](result: ExprResult[G, _, _, _]): Seq[(String, Json)] =
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
