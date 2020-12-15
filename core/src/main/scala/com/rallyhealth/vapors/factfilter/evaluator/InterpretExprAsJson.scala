package com.rallyhealth.vapors.factfilter.evaluator

import cats.data.NonEmptyList
import cats.kernel.Monoid
import cats.{FlatMap, Foldable, Functor, Show, Traverse}
import com.rallyhealth.vapors.core.algebra.Expr
import com.rallyhealth.vapors.core.data.Window
import com.rallyhealth.vapors.core.logic.{Conjunction, Disjunction, Negation}
import com.rallyhealth.vapors.core.math.{Addition, Negative, Subtraction}
import com.rallyhealth.vapors.factfilter.data.ExtractBoolean
import com.rallyhealth.vapors.factfilter.data.circe._
import io.circe.syntax._
import io.circe.{Json, JsonObject}

object InterpretExprAsJson {

  type Out[R] = Json

  def encode[F[_], V, R, P](expr: Expr[F, V, R, P]): Json = expr.visit(new InterpretExprAsJson())

  // This takes a higher-kinded parameter because there isn't a good way to use a wild-card
  // See https://github.com/scala/bug/issues/8039 for more details
  private[vapors] def commonJsonEncoding[F[_]](
    expr: Expr[F, _, _, _],
    additionalFields: Seq[(String, Json)] = Seq.empty,
  ): Json = {
    var obj = {
      JsonObject(
        "node" -> expr.getClass.getSimpleName.asJson,
      )
    }
    obj = {
      if (additionalFields.nonEmpty) JsonObject.fromIterable(additionalFields).deepMerge(obj)
      else obj
    }
    Json.fromJsonObject(obj)
  }
}

// TODO: Make it optional to stop encoding all subExpressions in the arguments list after a top-level expression?
//       Or maybe only print the input expression of each node (and arguments will encode all subExpressions)?
// TODO: Make it optional to unhide Embed node wrappers and just show their sub expression
// TODO: Make it optional to hide ReturnInput nodes
class InterpretExprAsJson[F[_], V, P](encodeSubExpressions: Boolean = true)
  extends Expr.Visitor[F, V, P, InterpretExprAsJson.Out] {
  import InterpretExprAsJson._

  override def visitAddOutputs[R : Addition](expr: Expr.AddOutputs[F, V, R, P]): Json = commonJsonEncoding(expr)

  override def visitAnd[R : Conjunction : ExtractBoolean](expr: Expr.And[F, V, R, P]): Json = {
    commonJsonEncoding(expr, skipOrEncodeMultiple(expr.inputExprList))
  }

  override def visitCollectSomeOutput[M[_] : Foldable, U, R : Monoid](
    expr: Expr.CollectFromOutput[F, V, M, U, R, P],
  ): Json =
    commonJsonEncoding(
      expr,
      skipOrEncodeSingle("collectSomeExpr" -> expr.collectExpr) ++ skipOrEncodeSingle("inputExpr" -> expr.inputExpr),
    )

  override def visitConstOutput[R](expr: Expr.ConstOutput[F, V, R, P]): Json = expr.value.toString.asJson

  override def visitDefine[M[_] : Foldable, T](expr: Expr.Define[M, T, P]): Json = commonJsonEncoding(expr)

  override def visitEmbed[R](expr: Expr.Embed[F, V, R, P]): Json = encode(expr.embeddedExpr)

  override def visitExistsInOutput[M[_] : Foldable, U](expr: Expr.ExistsInOutput[F, V, M, U, P]): Json = {
    commonJsonEncoding(
      expr,
      skipOrEncodeSingle("conditionExpr" -> expr.conditionExpr) ++ skipOrEncodeSingle("inputExpr" -> expr.inputExpr),
    )
  }

  override def visitFlatMapOutput[M[_] : Foldable : FlatMap, U, X](expr: Expr.FlatMapOutput[F, V, M, U, X, P]): Json =
    commonJsonEncoding(expr)

  override def visitMapOutput[M[_] : Foldable : Functor, U, R](expr: Expr.MapOutput[F, V, M, U, R, P]): Json =
    commonJsonEncoding(expr)

  override def visitNegativeOutput[R : Negative](expr: Expr.NegativeOutput[F, V, R, P]): Json =
    commonJsonEncoding(expr, skipOrEncodeSingle(expr.inputExpr))

  override def visitNot[R : Negation](expr: Expr.Not[F, V, R, P]): Json = {
    commonJsonEncoding(expr, skipOrEncodeSingle(expr.inputExpr))
  }

  override def visitOr[R : Disjunction : ExtractBoolean](expr: Expr.Or[F, V, R, P]): Json = {
    commonJsonEncoding(expr, skipOrEncodeMultiple(expr.inputExprList))
  }

  override def visitOutputWithinWindow[R](expr: Expr.OutputWithinWindow[F, V, R, P]): Json = {
    val windowAsString = Window.showWindowWithTerm[R]("input")(Show.fromToString).show(expr.window)
    commonJsonEncoding(
      expr,
      Seq(
        "comparison" -> windowAsString.asJson,
      ) ++ skipOrEncodeSingle("inputExpr" -> expr.inputExpr),
    )
  }

  override def visitReturnInput(expr: Expr.ReturnInput[F, V, P]): Json = "return input".asJson

  override def visitSelectFromOutput[S, R](expr: Expr.SelectFromOutput[F, V, S, R, P]): Json = commonJsonEncoding(expr)

  override def visitSubtractOutputs[R : Subtraction](expr: Expr.SubtractOutputs[F, V, R, P]): Json =
    commonJsonEncoding(expr)

  override def visitUsingDefinitions[R](expr: Expr.UsingDefinitions[F, V, R, P]): Json = commonJsonEncoding(expr)

  override def visitWhen[R](expr: Expr.When[F, V, R, P]): Json = commonJsonEncoding(expr)

  override def visitWithFactsOfType[T, R](expr: Expr.WithFactsOfType[T, R, P]): Json = {
    val factTypesJson = expr.factTypeSet.asJson
    commonJsonEncoding(
      expr,
      Seq(
        "factTypes" -> factTypesJson,
      ) ++ skipOrEncodeSingle(expr.subExpr),
    )
  }

  // These take a higher-kinded parameter because there isn't a good way to use a wild-card
  // See https://github.com/scala/bug/issues/8039 for more details

  private def skipOrEncodeSingle[G[_]](expr: Expr[G, _, _, _]): Seq[(String, Json)] =
    skipOrEncodeSingle("subExpression" -> expr)

  private def skipOrEncodeSingle[G[_]](t: (String, Expr[G, _, _, _])): Seq[(String, Json)] = {
    if (encodeSubExpressions) t match {
      case (name, expr) if !expr.isInstanceOf[Expr.ReturnInput[G, _, _]] =>
        Seq(name -> encode(expr))
      case _ => Nil
    } else Nil
  }

  private def skipOrEncodeMultiple[G[_]](exprs: NonEmptyList[Expr[G, _, _, _]]): Seq[(String, Json)] =
    skipOrEncodeMultiple("subExpressions" -> exprs)

  private def skipOrEncodeMultiple[G[_]](t: (String, NonEmptyList[Expr[G, _, _, _]])): Seq[(String, Json)] = {
    if (encodeSubExpressions) t match {
      case (name, exprs) =>
        val filteredExpressions = exprs.toList.flatMap {
          case Expr.ReturnInput(_) => None
          case keep => Some(encode(keep))
        }
        if (filteredExpressions.isEmpty) Nil
        else Seq(name -> filteredExpressions.asJson)
    } else Nil
  }

}
