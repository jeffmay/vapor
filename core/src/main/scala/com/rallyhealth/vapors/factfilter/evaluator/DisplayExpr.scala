package com.rallyhealth.vapors.factfilter.evaluator

import cats.data.Chain
import cats.kernel.Monoid
import cats.{FlatMap, Foldable, Functor, FunctorFilter, Semigroupal, Show, Traverse, TraverseFilter}
import com.rallyhealth.vapors.core.algebra.Expr
import com.rallyhealth.vapors.core.data.Window
import com.rallyhealth.vapors.core.logic.{Conjunction, Disjunction, Negation}
import com.rallyhealth.vapors.core.math.{Addition, Negative, Subtraction}
import com.rallyhealth.vapors.factfilter.data.ExtractBoolean
import shapeless.HList

object DisplayExpr {

  def serialize[F[_], V, R, P](expr: Expr[F, V, R, P]): String =
    expr.visit(new DisplayExpr(newline = "", indentWidth = 0)).iterator.mkString

  def prettyPrint[F[_], V, R, P](expr: Expr[F, V, R, P]): String = expr.visit(new DisplayExpr()).iterator.mkString

  private[DisplayExpr] type G[R] = Out

  private[DisplayExpr] type Out = Chain[String]
}

final case class DisplayExpr[F[_], V, P](
  newline: String = "\n",
  whitespace: String = " ",
  newlineBetweenParens: Boolean = true,
  indentWidth: Int = 2,
  spaceAfterMethod: Boolean = false,
  indentLevel: Int = 0,
) extends Expr.Visitor[F, V, P, DisplayExpr.G] {

  import DisplayExpr._
  import cats.syntax.show._

  private def ws: String = whitespace
  private val i: String = ws * (indentLevel * indentWidth)
  private def nl: String = newline
  private val nli: String = nl + i
  private val nlii: String = nli + i
  private def ap: String = if (newlineBetweenParens) nli else ""
  private def bp: String = if (newlineBetweenParens) nli else ""
  private def ac: String = whitespace
  private def am: String = if (spaceAfterMethod) ws else ""
  private def chain: String = nlii

  private def serialize[G[_], U, R](
    expr: Expr[G, U, R, P],
    indentLevel: Int = this.indentLevel + 1,
  ): Out =
    expr.visit(copy(indentLevel = indentLevel))

  private val comma = Chain(s",$ac")

  private implicit object joinOutWithCommas extends Functor[G] with Semigroupal[G] {
    override def map[A, B](fa: G[A])(f: A => B): G[B] = fa
    override def product[A, B](
      fa: G[A],
      fb: G[B],
    ): G[(A, B)] = fa ++ comma ++ fb
  }

  override def visitConstOutput[R](expr: Expr.ConstOutput[F, V, R, P]): Out =
    Chain(s"${i}const$am($ap${expr.value},$ac${expr.evidence})")

  override def visitReturnInput(expr: Expr.ReturnInput[F, V, P]): Out = Chain(s"${i}return input")

  override def visitCollectSomeOutput[M[_] : Foldable, U, R : Monoid](
    expr: Expr.CollectFromOutput[F, V, M, U, R, P],
  ): Out = {
    s"${i}collectFrom$am($ap" +: (serialize(expr.inputExpr) ++ comma ++ serialize(expr.collectExpr)) :+ s"$bp)"
  }

  override def visitAddOutputs[R : Addition](expr: Expr.AddOutputs[F, V, R, P]): Out =
    expr.inputExprList.map(serialize(_)).reduceLeft(_ ++ Chain(" + ") ++ _)

  override def visitSubtractOutputs[R : Subtraction](expr: Expr.SubtractOutputs[F, V, R, P]): Out =
    expr.inputExprList.map(serialize(_)).reduceLeft(_ ++ Chain(" - ") ++ _)

  override def visitFilterOutput[M[_] : Foldable : FunctorFilter, R](expr: Expr.FilterOutput[F, V, M, R, P]): Out = {
    s"${i}filterFrom$am($ap" +: (serialize(expr.inputExpr) ++ comma ++ serialize(expr.condExpr)) :+ s"$bp)"
  }

  override def visitFlatMapOutput[M[_] : Foldable : FlatMap, U, X](
    expr: Expr.FlatMapOutput[F, V, M, U, X, P],
  ): Out = {
    s"${i}flatMapFrom$am($ap" +: (serialize(expr.inputExpr) ++ comma ++ serialize(expr.flatMapExpr)) :+ s"$bp)"
  }

  override def visitSelectFromOutput[S, R](expr: Expr.SelectFromOutput[F, V, S, R, P]): Out = {
    val lensPath = expr.lens.path
    if (lensPath.isEmpty) s"${i}selectFrom$am($ap" +: serialize(expr.inputExpr) :+ s"$bp)"
    else s"${i}selectFrom$am(${ap}_${expr.lens.path.show},$ac" +: serialize(expr.inputExpr) :+ s"$bp)"
  }

  override def visitMapOutput[M[_] : Foldable : Functor, U, R](expr: Expr.MapOutput[F, V, M, U, R, P]): Out = {
    s"${i}mapFrom$am($ap" +: (serialize(expr.inputExpr) ++ comma ++ serialize(expr.mapExpr)) :+ s"$bp)"
  }

  override def visitTakeFromOutput[M[_] : Traverse : TraverseFilter, R](
    expr: Expr.TakeFromOutput[F, V, M, R, P],
  ): Out = {
    s"${i}takeFrom$am($ap" +: (serialize(expr.inputExpr) ++ comma) :+ s",$ac" :+ expr.take.toString :+ s"$bp)"
  }

  override def visitWrapOutput[T <: HList, R](expr: Expr.WrapOutput[F, V, T, R, P]): Out = {
    val expressions = expr.inputExprHList.visit(DisplayExpr())
    s"($ap" +: expressions :+ s"$bp)${expr.converter.conversionType}"
  }

  override def visitEmbed[R](expr: Expr.Embed[F, V, R, P]): Out = serialize(expr.embeddedExpr)

  override def visitDefine[M[_] : Foldable, T](expr: Expr.Define[M, T, P]): Out = {
    s"${i}declare$am($ap'${expr.factType}' -> '" +: serialize(expr.definitionExpr) :+ s"$bp)"
  }

  override def visitWithFactsOfType[T, R](self: Expr.WithFactsOfType[T, R, P]): Out = {
    val factTypeStrings = self.factTypeSet.typeSet.toSortedSet.map(_.toString).mkString(" or ")
    s"${i}withFactsOfType$am($ap" +: factTypeStrings +: s"$bp)$chain.build$am($ap" +: serialize(self.subExpr) :+ s"$bp)"
  }

  override def visitUsingDefinitions[R](expr: Expr.UsingDefinitions[F, V, R, P]): Out = {
    s"${i}using$am($ap" +: expr.definitions.map(serialize(_)).reduceLeft(_ ++ comma ++ _)
  } ++ {
    s"$bp)$chain.andThen$am($ap" +: serialize(expr.subExpr) :+ s"$bp)"
  }

  override def visitAnd[R : Conjunction : ExtractBoolean](expr: Expr.And[F, V, R, P]): Out = {
    s"${i}and$am($ap" +: expr.inputExprList.map(serialize(_)).reduceLeft(_ ++ comma ++ _) :+ s"$bp)"
  }

  override def visitOr[R : Disjunction : ExtractBoolean](expr: Expr.Or[F, V, R, P]): Out = {
    s"${i}or$am($ap" +: expr.inputExprList.map(serialize(_)).reduceLeft(_ ++ comma ++ _) :+ s"$bp)"
  }

  override def visitOutputIsEmpty[M[_] : Foldable, R](expr: Expr.OutputIsEmpty[F, V, M, R, P]): Out = {
    s"${i}isEmpty$am($ap" +: serialize(expr.inputExpr) :+ s"$bp)"
  }

  override def visitOutputWithinSet[R](expr: Expr.OutputWithinSet[F, V, R, P]): Out = {
    s"${i}within$am($ap" +: serialize(expr.inputExpr)
  } ++ comma :+ {
    val acceptedValuesAsString = expr.accepted.mkString("[", s",$ac", "]")
    s",$ac$acceptedValuesAsString$bp)"
  }

  override def visitOutputWithinWindow[R](expr: Expr.OutputWithinWindow[F, V, R, P]): Out = {
    implicit def showR: Show[R] = Show.fromToString
    val windowAsString = Window.showWindow[R].show(expr.window)
    s"${i}within$am($ap" +: serialize(expr.inputExpr) :+ s",$ac" :+ windowAsString :+ s"$bp)"
  }

  override def visitNegativeOutput[R : Negative](expr: Expr.NegativeOutput[F, V, R, P]): Out = {
    "-" +: serialize(expr.inputExpr)
  }

  override def visitNot[R](expr: Expr.Not[F, V, R, P])(implicit RN: Negation[R]): Out = {
    s"${i}not$am($ap" +: serialize(expr.inputExpr) :+ s"$bp)"
  }

  override def visitWhen[R](expr: Expr.When[F, V, R, P]): Out = {
    val branchChains = expr.conditionBranches.map { branch =>
      val whenChain = serialize(branch.whenExpr)
      val thenChain = serialize(branch.thenExpr)
      (s"($ap" +: whenChain :+ s"$bp)$chain.then$am($ap") ++ (thenChain :+ s"$bp)")
    }
    val firstWhenThen = s"${i}when$am($ap" +: branchChains.head
    val elifThens = branchChains.tail.foldLeft(Chain.empty[String]) {
      case (acc, next) =>
        (acc :+ s"$chain.elif$am($ap") ++ next
    }
    val finalElse = s"$chain.else$am($ap" +: serialize(expr.defaultExpr)
    firstWhenThen ++ elifThens ++ finalElse
  }

  override def visitExistsInOutput[M[_] : Foldable, U](expr: Expr.ExistsInOutput[F, V, M, U, P]): Out = {
    s"${i}exists$am($ap" +: serialize(expr.inputExpr)
  } ++ {
    s"$bp)$chain.where$am($ap" +: serialize(expr.conditionExpr)
  } :+ s"$bp)"

}
