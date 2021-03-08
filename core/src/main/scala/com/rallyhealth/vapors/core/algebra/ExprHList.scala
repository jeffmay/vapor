package com.rallyhealth.vapors.core.algebra

import cats.syntax.all._
import cats.{Align, Functor, Semigroupal}
import com.rallyhealth.vapors.core.data.FactTable
import com.rallyhealth.vapors.core.dsl.{const, eval}
import shapeless.{::, HList, HNil}

// TODO: Is there any need for V and P?
sealed trait ExprHList[V, P]

object ExprHList {
  import scala.language.implicitConversions

  @inline final def nil[V, P]: ExprNil[V, P] = ExprNil()

  @inline final def nilRoot[P]: ExprNil[FactTable, P] = ExprNil()

  implicit final class Ops[V, P, E <: ExprHList[V, P]](l: E) {

    def ::[H](expr: Expr[V, H, P]): ExprCons[V, H, E, P] = ExprCons(expr, l)

    def zipWithDefaults[M[_] : Align : Functor, DL <: HList](
      implicit
      op: CanZipWithDefaults[V, P, M, DL, E],
    ): ZipWithDefaults[M, DL] = new ZipWithDefaults[M, DL]

    final class ZipWithDefaults[M[_] : Align : Functor, DL <: HList](implicit op: CanZipWithDefaults[V, P, M, DL, E]) {

      def using[G[_] : Functor : Semigroupal](
        defaultsExpr: Expr[V, DL, P],
      )(implicit
        captureResult: CaptureP[V, M[DL], P],
      ): Expr[V, M[DL], P] =
        Expr.ZipWithDefaults(l, defaultsExpr, captureResult)
    }
  }
}

final case class ExprCons[V, IRH, +TEL <: ExprHList[V, P], P](
  head: Expr[V, IRH, P],
  tail: TEL,
) extends ExprHList[V, P]

object ExprCons {

  implicit def ops[V, H, T <: ExprHList[V, P], P](l: ExprCons[V, H, T, P]): ExprHList.Ops[V, P, ExprCons[V, H, T, P]] =
    new ExprHList.Ops(l)
}

sealed abstract class ExprNil[V, P] extends ExprHList[V, P] {
  def ::[H](expr: Expr[V, H, P]): ExprCons[V, H, ExprNil[V, P], P] = ExprCons(expr, this)
  override def toString: String = "ExprNil"
}

object ExprNil {
  type RootUnit = ExprNil[FactTable, Unit]
  final val instance = new ExprNil[Any, Any] {}
  @inline final def apply[V, P](): ExprNil[V, P] = instance.asInstanceOf[ExprNil[V, P]]
  implicit def ops[V, P](l: ExprNil[V, P]): ExprHList.Ops[V, P, ExprNil[V, P]] = new ExprHList.Ops(l)
}

/**
  * @tparam F TODO: Needed? Or can I use LiftAll at the call site?
  * @tparam DL (aka defaults list) - an [[HList]] of the default values to use when padding the shorter lists
  * @tparam IEL (aka input expression list) - a [[ExprHList]] of the input expressions
  */
trait CanZipWithDefaults[V, P, F[_], DL <: HList, IEL <: ExprHList[V, P]] {

  def applyZipWithDefaults[G[_] : Functor : Semigroupal](
    defaults: DL,
    inputExpr: IEL,
    v: Expr.Visitor[V, P, G],
  ): G[F[DL]]
}

object CanZipWithDefaults {

  implicit def hlist1CanZipWithDefaults[
    V,
    P,
    M[_] : Functor,
    RH,
  ]: CanZipWithDefaults[V, P, M, RH :: HNil, ExprCons[V, M[RH], ExprNil[V, P], P]] = {
    new CanZipWithDefaults[V, P, M, RH :: HNil, ExprCons[V, M[RH], ExprNil[V, P], P]] {
      override def applyZipWithDefaults[G[_] : Functor : Semigroupal](
        defaults: RH :: HNil,
        inputExpr: ExprCons[V, M[RH], ExprNil[V, P], P],
        v: Expr.Visitor[V, P, G],
      ): G[M[RH :: HNil]] = {
        inputExpr.head.visit(v).map(_.map(_ :: HNil))
      }
    }
  }

  /**
    *
    * @tparam M the alignable type
    * @tparam RH the returned HList head
    * @tparam TEL the tail [[ExprHList]] of the remaining expressions to be zipped
    * @tparam RT an [[HList]] of default values that match the wrapped-types of the [[RTM]] defined above
    */
  implicit def hlistCanZipWithDefaults[V, P, M[_] : Align : Functor, RH, RT <: HNil, TEL <: ExprHList[V, P]](
    implicit
    zip: CanZipWithDefaults[V, P, M, RT, TEL],
  ): CanZipWithDefaults[V, P, M, RH :: RT, ExprCons[V, M[RH], TEL, P]] = {
    new CanZipWithDefaults[V, P, M, RH :: RT, ExprCons[V, M[RH], TEL, P]] {

      override def applyZipWithDefaults[G[_] : Functor : Semigroupal](
        defaults: RH :: RT,
        inputExpr: ExprCons[V, M[RH], TEL, P],
        v: Expr.Visitor[V, P, G],
      ): G[M[RH :: RT]] = {
        val headRes = inputExpr.head.visit(v)
        val tailRes = zip.applyZipWithDefaults(defaults.tail, inputExpr.tail, v)
        val z: G[M[RH :: RT]] = (headRes, tailRes).mapN { (mh, mt) =>
          val x: M[(RH, RT)] = mh.zipAll(mt, defaults.head, defaults.tail)
          val y: M[RH :: RT] = x.map {
            case (h, t) => ::(h, t)
          }
          y
        }
        z
      }
    }
  }
}

object Example {

  def main(args: Array[String]): Unit = {
    type E = ExprCons[FactTable, List[String], ExprCons[FactTable, List[Int], ExprNil[FactTable, Unit], Unit], Unit]
    val hl: E =
      const(List("A", "B")) :: const(List(1, 2, 3)) :: ExprHList.nilRoot[Unit]

    // TODO: make this work...
    implicit def op: CanZipWithDefaults[FactTable, Unit, List, String :: Int :: HNil, E] = ???
    val q = Expr.ZipWithDefaults[FactTable, List, String :: Int :: HNil, E, Unit](
      hl,
      const("Z" :: 0 :: HNil),
      CaptureP.captureUnit,
    )
    val result = eval(FactTable.empty)(q)
    println(result)
  }
}
