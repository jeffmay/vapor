package com.rallyhealth.vapors.circe.interpreter

import cats.data.NonEmptyList
import cats.kernel.Monoid
import cats.{Align, FlatMap, Foldable, Functor, FunctorFilter, MonoidK, Order, Show, Traverse, TraverseFilter}
import com.rallyhealth.vapors.circe._
import com.rallyhealth.vapors.core.algebra.{Expr, ExprHCons, ExprLast, NonEmptyExprHList}
import com.rallyhealth.vapors.core.data.{ExtractBoolean, Window}
import com.rallyhealth.vapors.core.logic.{Conjunction, Disjunction, Negation}
import com.rallyhealth.vapors.core.math.{Addition, Division, Multiplication, Negative, Subtraction}
import io.circe.syntax._
import io.circe.{Json, JsonObject}
import shapeless.HList

import scala.annotation.tailrec
import scala.collection.MapView

object InterpretExprAsJson {

  type Out[R] = Json

  def encode[V, R, P](expr: Expr[V, R, P]): Json = expr.visit(new InterpretExprAsJson())

  private[vapors] def commonJsonEncoding(
    expr: Expr[_, _, _],
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
class InterpretExprAsJson[V, P](encodeSubExpressions: Boolean = true)
  extends Expr.Visitor[V, P, InterpretExprAsJson.Out] {
  import InterpretExprAsJson._

  override def visitAddOutputs[R : Addition](expr: Expr.AddOutputs[V, R, P]): Json = commonJsonEncoding(expr)

  override def visitAnd[R : Conjunction : ExtractBoolean](expr: Expr.And[V, R, P]): Json = {
    commonJsonEncoding(expr, skipOrEncodeSubExprList(expr.inputExprList.toList))
  }

  override def visitCollectSomeOutput[M[_] : Foldable, U, R : Monoid](
    expr: Expr.CollectFromOutput[V, M, U, R, P],
  ): Json =
    commonJsonEncoding(
      expr,
      skipOrEncodeSubExpr("collectSomeExpr", expr.collectExpr) ++
        skipOrEncodeInputExpr(expr.inputExpr),
    )

  override def visitConcatOutput[M[_] : MonoidK, R](expr: Expr.ConcatOutput[V, M, R, P]): Out[M[R]] =
    commonJsonEncoding(expr, skipOrEncodeInputExprList(expr.inputExprList.toList))

  override def visitCustomFunction[A, R](expr: Expr.CustomFunction[V, A, R, P]): Out[R] =
    commonJsonEncoding(expr, skipOrEncodeInputExpr(expr.inputExpr))

  override def visitTakeFromOutput[M[_] : Traverse : TraverseFilter, R](expr: Expr.TakeFromOutput[V, M, R, P]): Json =
    commonJsonEncoding(
      expr,
      Seq(
        "take" -> expr.take.asJson,
      ) ++ skipOrEncodeInputExpr(expr.inputExpr),
    )

  override def visitConstOutput[R](expr: Expr.ConstOutput[V, R, P]): Json = expr.value.toString.asJson

  override def visitDefine[M[_] : Foldable, T](expr: Expr.Define[M, T, P]): Json = commonJsonEncoding(expr)

  override def visitDivideOutputs[R : Division](expr: Expr.DivideOutputs[V, R, P]): Out[R] =
    commonJsonEncoding(expr, skipOrEncodeInputExprList(expr.inputExprList.toList))

  override def visitEmbed[R](expr: Expr.Embed[V, R, P]): Json = encode(expr.embeddedExpr)

  override def visitExistsInOutput[M[_] : Foldable, U](expr: Expr.ExistsInOutput[V, M, U, P]): Json = {
    commonJsonEncoding(
      expr,
      skipOrEncodeSubExpr("conditionExpr", expr.conditionExpr) ++
        skipOrEncodeInputExpr(expr.inputExpr),
    )
  }

  override def visitFlatMapOutput[M[_] : Foldable : FlatMap, U, X](expr: Expr.FlatMapOutput[V, M, U, X, P]): Json =
    commonJsonEncoding(expr)

  override def visitFilterOutput[M[_] : Foldable : FunctorFilter, R](expr: Expr.FilterOutput[V, M, R, P]): Json =
    commonJsonEncoding(
      expr,
      skipOrEncodeSubExpr("filterExpr", expr.condExpr) ++
        skipOrEncodeInputExpr(expr.inputExpr),
    )

  override def visitFoldOutput[M[_] : Foldable, R : Monoid](expr: Expr.FoldOutput[V, M, R, P]): Out[R] =
    commonJsonEncoding(expr, skipOrEncodeInputExpr(expr.inputExpr))

  override def visitGroupOutput[M[_] : Foldable, U : Order, K](
    expr: Expr.GroupOutput[V, M, U, K, P],
  ): Out[MapView[K, Seq[U]]] =
    commonJsonEncoding(expr, skipOrEncodeInputExpr(expr.inputExpr))

  override def visitMapOutput[M[_] : Foldable : Functor, U, R](expr: Expr.MapOutput[V, M, U, R, P]): Json =
    commonJsonEncoding(expr)

  override def visitMultiplyOutputs[R : Multiplication](expr: Expr.MultiplyOutputs[V, R, P]): Out[R] =
    commonJsonEncoding(expr, skipOrEncodeInputExprList(expr.inputExprList.toList))

  override def visitNegativeOutput[R : Negative](expr: Expr.NegativeOutput[V, R, P]): Json =
    commonJsonEncoding(expr, skipOrEncodeSubExpr(expr.inputExpr))

  override def visitNot[R : Negation](expr: Expr.Not[V, R, P]): Json = {
    commonJsonEncoding(expr, skipOrEncodeSubExpr(expr.inputExpr))
  }

  override def visitOr[R : Disjunction : ExtractBoolean](expr: Expr.Or[V, R, P]): Json = {
    commonJsonEncoding(expr, skipOrEncodeSubExprList(expr.inputExprList.toList))
  }

  override def visitOutputIsEmpty[M[_] : Foldable, R](expr: Expr.OutputIsEmpty[V, M, R, P]): Json =
    commonJsonEncoding(
      expr,
      skipOrEncodeInputExpr(expr.inputExpr),
    )

  override def visitOutputWithinSet[R](expr: Expr.OutputWithinSet[V, R, P]): Json =
    commonJsonEncoding(
      expr,
      Seq(
        // TODO: Use Json encoding instead of toString?
        "accepted" -> expr.accepted.toSeq.map(_.toString).asJson,
      ) ++ skipOrEncodeInputExpr(expr.inputExpr),
    )

  override def visitSortOutput[M[_], R](expr: Expr.SortOutput[V, M, R, P]): Out[M[R]] =
    commonJsonEncoding(
      expr,
      Seq(
        "sorter" -> expr.sorter.sortDescription.asJson,
      ) ++ skipOrEncodeInputExpr(expr.inputExpr),
    )

  override def visitOutputWithinWindow[R](expr: Expr.OutputWithinWindow[V, R, P]): Json = {
    val windowAsString = Window.showWindowWithTerm[R]("input")(Show.fromToString).show(expr.window)
    commonJsonEncoding(
      expr,
      Seq(
        "comparison" -> windowAsString.asJson,
      ) ++ skipOrEncodeInputExpr(expr.inputExpr),
    )
  }

  override def visitReturnInput(expr: Expr.ReturnInput[V, P]): Json = "return input".asJson

  override def visitSelectFromOutput[S, R](expr: Expr.SelectFromOutput[V, S, R, P]): Json = commonJsonEncoding(expr)

  override def visitSubtractOutputs[R : Subtraction](expr: Expr.SubtractOutputs[V, R, P]): Json =
    commonJsonEncoding(expr)

  override def visitUsingDefinitions[R](expr: Expr.UsingDefinitions[V, R, P]): Json = commonJsonEncoding(expr)

  override def visitWhen[R](expr: Expr.When[V, R, P]): Json = commonJsonEncoding(expr)

  override def visitWithFactsOfType[T, R](expr: Expr.WithFactsOfType[T, R, P]): Json = {
    val factTypesJson = expr.factTypeSet.asJson
    commonJsonEncoding(
      expr,
      Seq(
        "factTypes" -> factTypesJson,
      ) ++ skipOrEncodeSubExpr(expr.subExpr),
    )
  }

  override def visitWrapOutputHList[T <: HList, R](expr: Expr.WrapOutputHList[V, T, R, P]): Out[R] =
    commonJsonEncoding(expr, skipOrEncodeInputExprList(exprHListAsList(expr.inputExprHList)))

  override def visitWrapOutputSeq[R](expr: Expr.WrapOutputSeq[V, R, P]): Out[Seq[R]] =
    commonJsonEncoding(expr, skipOrEncodeInputExprList(expr.inputExprList))

  override def visitZipOutput[M[_] : Align : FunctorFilter, L <: HList, R](
    expr: Expr.ZipOutput[V, M, L, R, P],
  ): Out[M[R]] =
    commonJsonEncoding(expr, skipOrEncodeInputExprList(exprHListAsList(expr.inputExprHList)))

  private def exprHListAsList[M[_]](hl: NonEmptyExprHList[_, M, _, _]): Seq[Expr[_, _, _]] = {
    @tailrec def appendAllExprs(
      hl: NonEmptyExprHList[_, M, _, _],
      acc: List[Expr[_, _, _]],
    ): List[Expr[_, _, _]] = hl match {
      case last: ExprLast[_, M, _, _] => last.last :: Nil
      case cons: ExprHCons[_, M, _, _, _] => appendAllExprs(cons.tail, cons.head :: acc)
    }
    appendAllExprs(hl, Nil)
  }

  private def skipOrEncodeInputExpr(expr: Expr[_, _, _]): Seq[(String, Json)] =
    skipOrEncodeInputExprList(Seq(expr))

  private def skipOrEncodeInputExprList(exprs: Seq[Expr[_, _, _]]): Seq[(String, Json)] =
    skipOrEncodeSubExprList("inputResults", exprs)

  private def skipOrEncodeSubExpr(expr: Expr[_, _, _]): Seq[(String, Json)] =
    skipOrEncodeSubExprList(Seq(expr))

  private def skipOrEncodeSubExpr(
    name: String,
    expr: Expr[_, _, _],
  ): Seq[(String, Json)] =
    skipOrEncodeSubExprList(name, Seq(expr))

  private def skipOrEncodeSubExprList(exprs: Seq[Expr[_, _, _]]): Seq[(String, Json)] =
    skipOrEncodeSubExprList("subExpressions", exprs)

  private def skipOrEncodeSubExprList(
    name: String,
    exprs: Seq[Expr[_, _, _]],
  ): Seq[(String, Json)] = {
    if (encodeSubExpressions) {
      val filteredExpressions = exprs.flatMap {
        case Expr.ReturnInput(_) => None
        case keep => Some(encode(keep))
      }
      if (filteredExpressions.isEmpty) Nil
      else Seq(name -> filteredExpressions.asJson)
    } else Nil
  }

}
