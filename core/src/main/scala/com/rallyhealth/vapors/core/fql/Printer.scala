package com.rallyhealth.vapors.core.fql

import cats.{~>, Applicative, Show}
import com.rallyhealth.vapors.core.algebra._
import com.rallyhealth.vapors.core.data.BoundedWindow
import com.rallyhealth.vapors.core.dsl.AnyExp

class Printer {
  import Printer._

  def serialize[T, A](exp: AnyExp[T, A]): String = {
    val parts = exp.foldMap(ImmutablePrinter())(ApplicablePrinterF)
    ("_" :: parts).mkString
  }
}

object Printer {

  private type Buffer = List[String]
  private type PrinterF[_] = Buffer

  private implicit object ApplicablePrinterF extends Applicative[PrinterF] {
    override def pure[A](x: A): PrinterF[A] = Nil
    override def ap[A, B](ff: PrinterF[A => B])(fa: PrinterF[A]): PrinterF[B] = ff ::: fa
  }

  private object ImmutablePrinter extends (ExpAlg[Any, *] ~> PrinterF) {
    override def apply[A](fa: ExpAlg[Any, A]): List[String] = {
      import cats.syntax.show._
      implicit val showAnyFromToString: Show[Any] = Show.fromToString
      fa match {
        case ExpPure(label, _) => "<" :: label :: ">" :: Nil
        case ExpSelectField(selector, sub) => selector.path.show :: " " :: sub.foldMap(this)
        case ExpForAll(_, sub, _, _) => ".forall(_" :: sub.foldMap(this) ::: ")" :: Nil
        case ExpExists(_, sub, _, _) => ".exists(_" :: sub.foldMap(this) ::: ")" :: Nil
        case ExpWithin(window: BoundedWindow[Any], _, _) => "where " :: window.show :: Nil
        case ExpWithin(window, _, _) => "within(" :: window.toString :: ")" :: Nil
        case ExpCollect(subtypeName, _, sub, _) =>
          ".collect { case f: " :: subtypeName :: " => f" :: sub.foldMap(this) ::: " }" :: Nil
        case ExpCond(condExp, thenExp, elseExp) =>
          "if " :: condExp.foldMap(this) ::: " then " :: thenExp.foldMap(this) ::: " else " :: elseExp.foldMap(this)
        case ExpAnd(_, subs) =>
          subs.map(_.foldMap(this)).foldLeft[List[String]](Nil) {
            case (Nil, next) => next
            case (acc, next) => acc ::: " and " :: next
          }
        case ExpOr(_, subs) =>
          subs.map(_.foldMap(this)).foldLeft[List[String]](Nil) {
            case (Nil, next) => next
            case (acc, next) => acc ::: " or " :: next
          }
      }
    }

    def apply[T](): ExpAlg[T, *] ~> PrinterF = this.asInstanceOf[ExpAlg[T, *] ~> PrinterF]
  }

}
