package com.rallyhealth.vapors.factfilter.evaluator

import cats.data.{Chain, NonEmptyList, State}
import cats.kernel.Monoid
import cats.{FlatMap, Foldable, Functor, Monad}
import com.rallyhealth.vapors.core.algebra.Expr
import com.rallyhealth.vapors.core.data.Window
import com.rallyhealth.vapors.core.logic.{Conjunction, Disjunction, Negation}
import com.rallyhealth.vapors.core.math.{Addition, Negative, Subtraction}
import com.rallyhealth.vapors.factfilter.data.ExtractBoolean

import scala.collection.immutable.IntMap

private final class SimpleExprVisitorAdaptor[O, F[_], V, P](visitor: SimpleExprVisitor[O])
  extends Expr.Visitor[F, V, P, SimpleExprVisitorAdaptor.G[O, *]] {
  type Out = O

  import cats.syntax.all._

  override def visitAddOutputs[R : Addition](expr: Expr.AddOutputs[F, V, R, P]): Out =
    visitor.visitNode(expr.nodeType, None, expr.inputExprList.toList.map(_.nodeType))

  override def visitAnd[R : Conjunction : ExtractBoolean](expr: Expr.And[F, V, R, P]): Out =
    visitor.visitNode(expr.nodeType, None, expr.inputExprList.toList.map(_.nodeType))

  override def visitCollectSomeOutput[M[_] : Foldable, U, R : Monoid](
    expr: Expr.CollectFromOutput[F, V, M, U, R, P],
  ): Out =
    visitor.visitNode(expr.nodeType, Some(expr.inputExpr.nodeType), expr.collectExpr.nodeType :: Nil)

  override def visitConstOutput[R](expr: Expr.ConstOutput[F, V, R, P]): Out =
    visitor.visitNode(expr.nodeType, None, Nil)

  override def visitDefine[M[_] : Foldable, T](expr: Expr.Define[M, T, P]): Out =
    visitor.visitNode(expr.nodeType, Some(expr.definitionExpr.nodeType), Nil)

  override def visitEmbed[R](expr: Expr.Embed[F, V, R, P]): Out = ???
  override def visitExistsInOutput[M[_] : Foldable, U](expr: Expr.ExistsInOutput[F, V, M, U, P]): Out = ???
  override def visitFlatMapOutput[M[_] : Foldable : FlatMap, U, X](expr: Expr.FlatMapOutput[F, V, M, U, X, P]): Out =
    ???
  override def visitMapOutput[M[_] : Foldable : Functor, U, R](expr: Expr.MapOutput[F, V, M, U, R, P]): Out = ???
  override def visitNegativeOutput[R : Negative](expr: Expr.NegativeOutput[F, V, R, P]): Out = ???
  override def visitNot[R : Negation](expr: Expr.Not[F, V, R, P]): Out = ???
  override def visitOr[R : Disjunction : ExtractBoolean](expr: Expr.Or[F, V, R, P]): Out = ???
  override def visitOutputWithinWindow[R](expr: Expr.OutputWithinWindow[F, V, R, P]): Out = ???
  override def visitReturnInput(expr: Expr.ReturnInput[F, V, P]): Out = ???
  override def visitSelectFromOutput[S, R](expr: Expr.SelectFromOutput[F, V, S, R, P]): Out = ???
  override def visitSubtractOutputs[R : Subtraction](expr: Expr.SubtractOutputs[F, V, R, P]): Out = ???
  override def visitUsingDefinitions[R](expr: Expr.UsingDefinitions[F, V, R, P]): Out = ???
  override def visitWhen[R](expr: Expr.When[F, V, R, P]): Out = ???
  override def visitWithFactsOfType[T, R](expr: Expr.WithFactsOfType[T, R, P]): Out = ???
}

object SimpleExprVisitorAdaptor {
  private type G[O, _] = O

  def apply[C[_]](visitor: SimpleExprVisitor[C]): VisitorBuilder[C] = new VisitorBuilder(visitor)

  final class VisitorBuilder[C[_]](private val v: SimpleExprVisitor[C]) extends AnyVal {

    def serialize[F[_], V, R, P](expr: Expr[F, V, R, P]): String = {
      val sb = new StringBuilder
      new SimpleExprVisitorAdaptor(v, ExprPrinterConfig.defaults)
      expr.visit(v)
    }
  }
}

/**
  * Visits the expression as if it were written in the embedded DSL, but with some flexibility.
  *
  * {{{
  *   withFactsOfType(FactTypes.Age).build {
  *     _.exists {
  *       _.get(_.select(_.value)) >= 18
  *     }
  *   }
  * }}}
  *
  * [][withFactsOfType][][(][{]['age' as String][}][)]
  *
  * {nodeNameLeft}{nodeNameText}{nodeNameRight}{lArgText}{lArgRight}{lSetText}{lSetRight}{r}
  */
trait SimpleExprVisitor[O] {

  def visitLogicalNode(
    exprType: Expr.LogicalNodeType,
    subExprList: NonEmptyList[O],
  ): O

  def visitConstOutput[V](
    value: V,
    evidence: Evidence,
  )

  def visitWithinWindow[V](
    inputExpr: O,
    window: Window[V],
  ): O

}

object SimpleExprVisitor {

  final case class NodePieces(
    nodeNameText: String,
    inputExprText: String,
    subExprSep: String,
  )

//  final case class NodeTextPieces(
//    nodeNameText: String,
//    nodeNameRight: String = "",
//    startArgsText: String = "(",
//    startArgsRight: String = "",
//    argSepLeft: String = "",
//    argSepText: String = ",",
//    argSepRight: String = " ",
//    endArgsLeft: String = "",
//    endArgsText: String = ")",
//  )
//
//  final case class InputExprPieces(
//    inputNodeLeft: String = "",
//    inputNodeText: String = "_.",
//    inputNodeRight: String = "",
//  )

  type Out = Chain[String]

  implicit def default: LetterColIntRowArgNameGenerator = LetterColIntRowArgNameGenerator
}

class LetterColIntRowArgNameGenerator extends SimpleExprVisitor[State[LetterColIntRowArgNameGenerator.ConfigState, *]] {
  import LetterColIntRowArgNameGenerator.ConfigState
  import cats.syntax.all._

//  override def visitMethodName(nodeType: Expr.NodeType): State[ConfigState, String] = {
//    for {
//      (c, _) <- State.get[ConfigState]
//    } yield nodeType match {
//      case Expr.WithFactsOfType => "withFactsOfType"
//      case Expr.ConstOutput => ""
//      case Expr.ReturnInput => "return"
//      case Expr.And => "and"
//      case Expr.Or => "or"
//      case Expr.AddOutputs => "withFactsOfType"
//      case Expr.SubtractOutputs => "and"
//    }
//  }

//  override def visitInputExpr(
//    nodeType: Expr.NodeType,
//    config: ExprPrinterConfig,
//  ): State[IntMap[Int], ExprPrinterConfig] = ???
//
//  override def visitInputExprList(
//    nodeTypeList: NonEmptyList[Expr.NodeType],
//    config: ExprPrinterConfig,
//  ): State[IntMap[Int], ExprPrinterConfig] = ???
//
//  override def visitSubExpr(
//    nodeType: Expr.NodeType,
//    config: ExprPrinterConfig,
//  ): State[IntMap[Int], ExprPrinterConfig] = ???
//
//  override def visitSubExprList(
//    nodeTypeList: NonEmptyList[Expr.NodeType],
//    config: ExprPrinterConfig,
//  ): State[IntMap[Int], ExprPrinterConfig] = ???
//
//  override def configFor(
//    nodeType: Expr.NodeType,
//    current: ExprPrinterConfig,
//  ): State[IntMap[Int], ExprPrinterConfig] =
//    for {
//      lastRowCountByColumnIndex <- State.get[IntMap[Int]]
//      colIdx = current.indent.length
//      colName = LetterColIntRowArgNameGenerator.getColumnName(colIdx)
//      rowNumber = lastRowCountByColumnIndex.getOrElse(colIdx, 1)
//      _ <- State.set(lastRowCountByColumnIndex + (colIdx -> (rowNumber + 1)))
//    } yield current.copy(inputArgName = s"$colName$rowNumber")
}

object LetterColIntRowArgNameGenerator extends LetterColIntRowArgNameGenerator {

  type ConfigState = (ExprPrinterConfig, IntMap[Int])

  @throws[IllegalArgumentException]("the given character is not a valid base26 character")
  private def shiftBase26ToAlphaAsciiCodePoint(c: Char): Char = {
    if (c.isDigit) ('a' + (c - '0')).toChar
    else if (c.isLetter) (c + 10).toChar
    else throw new IllegalArgumentException(s"'$c' is not a valid base26 character")
  }

  def getColumnName(columnIndex: Int): String = {
    val base26 = Integer.toUnsignedString(columnIndex, 26)
    val sb = new StringBuilder(base26.length)
    for (c <- base26) {
      sb.append(shiftBase26ToAlphaAsciiCodePoint(c))
    }
    // long numbers start with 1 (which is translated to 'b'), but I want them to start with 'a'
    // so I must move the 1 into the 0 place for the first character of long numbers
    if (base26.length > 1) {
      sb.setCharAt(0, (sb.charAt(0) - 1).toChar)
    }
    sb.result()
  }
}

final case class ExprPrinterConfig(
  indent: String,
  inputArgName: String,
  lParenLeft: String = "",
  lParenRight: String = "",
  rParenLeft: String = "",
  rParenRight: String = "",
  methodNameLeft: String = ".",
  methodNameRight: String = "",
  lArgListText: String = "(",
  lArgListRight: String = "",
  rArgListLeft: String = "",
  rArgListText: String = ")",
  lSetText: String = "{",
  lSetRight: String = "",
  rSetLeft: String = "",
  rSetText: String = "}",
  lArrayText: String = "[",
  lArrayRight: String = "",
  rArrayLeft: String = "",
  rArrayText: String = "[",
  arrayCommaLeft: String = "",
  arrayCommaRight: String = "",
)

object ExprPrinterConfig {
  final val defaults = ExprPrinterConfig("", "")
}
