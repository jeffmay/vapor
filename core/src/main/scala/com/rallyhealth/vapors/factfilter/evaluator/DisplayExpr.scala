package com.rallyhealth.vapors.factfilter.evaluator

import cats.kernel.Monoid
import cats.{FlatMap, Foldable, Functor, Show, Traverse}
import com.rallyhealth.vapors.core.algebra.Expr
import com.rallyhealth.vapors.core.data.Window
import com.rallyhealth.vapors.core.logic.{Conjunction, Disjunction, Negation}
import com.rallyhealth.vapors.core.math.{Addition, Negative, Subtraction}
import com.rallyhealth.vapors.factfilter.data.ExtractBoolean
import com.rallyhealth.vapors.factfilter.evaluator.DisplayExpr.Output

import scala.collection.immutable.Queue

object DisplayExpr {

  def serialize[F[_], V, R, P](expr: Expr[F, V, R, P]): String =
    expr.visit(new DisplayExpr(newline = "", indentWidth = 0)).mkString

  def prettyPrint[F[_], V, R, P](expr: Expr[F, V, R, P]): String = expr.visit(new DisplayExpr()).mkString

  // TODO: Use chain?
  private[DisplayExpr] type Output[R] = Queue[String]
}

final case class DisplayExpr[F[_], V, P](
  newline: String = "\n",
  whitespace: String = " ",
  indentWidth: Int = 2,
  spaceAfterMethod: Boolean = false,
  indentLevel: Int = 0,
) extends Expr.Visitor[F, V, P, DisplayExpr.Output] {

  private def ws: String = whitespace
  private val i: String = ws * (indentLevel * indentWidth)
  private def nl: String = newline
  private val nli: String = nl + i
  private def ap: String = nli
  private def bp: String = nli
  private def ac: String = nli
  private def am: String = if (spaceAfterMethod) ws else ""
  private def beq: String = ws
  private def aeq: String = ws
  private val eq: String = s"$beq=$aeq"

  private def serialize[G[_], U, R](
    expr: Expr[G, U, R, P],
    indentLevel: Int = this.indentLevel + 1,
  ): Queue[String] =
    expr.visit(copy(indentLevel = indentLevel))

  override def visitConstOutput[R](expr: Expr.ConstOutput[F, V, R, P]): Queue[String] =
    Queue(s"pure$am($ap${expr.value}$bp)")

  override def visitReturnInput(expr: Expr.ReturnInput[F, V, P]): Output[F[V]] = Queue("return input")

  override def visitCollectSomeOutput[M[_] : Foldable, U, R : Monoid](
    expr: Expr.CollectFromOutput[F, V, M, U, R, P],
  ): Output[R] =
    Queue("collect(...)")

  override def visitAddOutputs[R : Addition](expr: Expr.AddOutputs[F, V, R, P]): Queue[String] = Queue("add(...)")

  override def visitSubtractOutputs[R : Subtraction](expr: Expr.SubtractOutputs[F, V, R, P]): Queue[String] =
    Queue("subtract(...)")

  override def visitFlatMapOutput[M[_] : Foldable : FlatMap, U, X](
    expr: Expr.FlatMapOutput[F, V, M, U, X, P],
  ): Output[M[X]] = Queue("flatMap(...)")

  override def visitOutputWithinWindow[R](expr: Expr.OutputWithinWindow[F, V, R, P]): Queue[String] = {
    implicit def showR: Show[R] = Show.fromToString
    val windowAsString = Window.showWindow[R].show(expr.window)
    s"within$am($ap" +: serialize(expr.inputExpr) :+ s",$ac" :+ windowAsString :+ s"$bp)"
  }

  override def visitSelectFromOutput[S, R](expr: Expr.SelectFromOutput[F, V, S, R, P]): Output[R] =
    Queue("selectFrom(...)")

  override def visitMapOutput[M[_] : Foldable : Functor, U, R](expr: Expr.MapOutput[F, V, M, U, R, P]): Queue[String] =
    Queue("mapEvery(...)")

  override def visitEmbed[R](expr: Expr.Embed[F, V, R, P]): Queue[String] = serialize(expr.embeddedExpr)

  override def visitDefine[M[_] : Foldable, T](expr: Expr.Define[M, T, P]): Queue[String] = {
    s"declare$am($ap'${expr.factType}' -> '" +: serialize(expr.definitionExpr) :+ s"$bp)"
  }

  override def visitWithFactsOfType[T, R](self: Expr.WithFactsOfType[T, R, P]): Queue[String] = {
    val factTypeStrings = self.factTypeSet.typeSet.toSortedSet.map(_.toString).mkString(" or ")
    s"withFactsOfType$am($ap" +: factTypeStrings +: s"$bp) return ($ap" +: serialize(self.subExpr) :+ s"$bp)"
  }

  override def visitUsingDefinitions[R](expr: Expr.UsingDefinitions[F, V, R, P]): Queue[String] = {
    s"using$am($ap" +: Queue.from(expr.definitions).flatMap(serialize(_) :+ s",$ac").dropRight(1)
  } ++ {
    s"$bp).andThen$am($ap" +: serialize(expr.subExpr) :+ s"$bp)"
  }

  override def visitAnd[R : Conjunction : ExtractBoolean](expr: Expr.And[F, V, R, P]): Queue[String] = {
    s"and$am($ap" +: expr.inputExprList.foldLeft(Queue.empty[String]) {
      case (acc, expr) =>
        (acc :+ s",$ac") ++ serialize(expr)
    } :+ s"$bp)"
  }

  override def visitOr[R : Disjunction : ExtractBoolean](expr: Expr.Or[F, V, R, P]): Queue[String] = {
    s"or$am($ap" +: expr.inputExprList.foldLeft(Queue.empty[String]) {
      case (acc, expr) =>
        (acc :+ s",$ac") ++ serialize(expr)
    } :+ s"$bp)"
  }

  override def visitNegativeOutput[R : Negative](expr: Expr.NegativeOutput[F, V, R, P]): Queue[String] = {
    "-" +: serialize(expr.inputExpr)
  }

  override def visitNot[R](expr: Expr.Not[F, V, R, P])(implicit RN: Negation[R]): Queue[String] = {
    s"not$am($ap" +: serialize(expr.inputExpr) :+ s"$bp)"
  }

  override def visitWhen[R](expr: Expr.When[F, V, R, P]): Queue[String] = {
    s"when$am(${ap}if$eq" +: serialize(expr.condExpr)
  } ++ {
    s",${ac}then$eq" +: serialize(expr.thenExpr)
  } ++ {
    s",${ac}else$eq" +: serialize(expr.elseExpr)
  } :+ s"$bp)"

  override def visitExistsInOutput[M[_] : Foldable, U](expr: Expr.ExistsInOutput[F, V, M, U, P]): Queue[String] = {
    s"exists$am($ap" +: serialize(expr.inputExpr)
  } ++ {
    s",$ac" +: serialize(expr.conditionExpr)
  } :+ s"$bp)"

}
