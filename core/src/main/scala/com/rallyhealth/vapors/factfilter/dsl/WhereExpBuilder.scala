package com.rallyhealth.vapors.factfilter.dsl

import cats.Eq
import cats.data.NonEmptyList
import cats.free.FreeApplicative
import com.rallyhealth.vapors.core.algebra._
import com.rallyhealth.vapors.core.data._
import com.rallyhealth.vapors.core.util.ReflectUtils.typeNameOf
import com.rallyhealth.vapors.factfilter.data._
import com.rallyhealth.vapors.factfilter.dsl

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

object CondBuilder {

  private final val any: CondBuilder[Any, Any] = new CondBuilder(NamedLens.id)

  def of[V]: CondBuilder[V, V] = any.asInstanceOf[CondBuilder[V, V]]

  final class IterableOps[T, C[_], V](private val wrap: CondExp[C[V]] => CondExp[T]) extends AnyVal {

    def exists(buildExp: CondBuilder[V, V] => CondExp[V])(implicit ev: C[V] <:< IterableOnce[V]): CondExp[T] = {
      val cond = buildExp(CondBuilder.of[V])
      wrap(dsl.exists(cond))
    }

    def forall(buildExp: CondBuilder[V, V] => CondExp[V])(implicit ev: C[V] <:< IterableOnce[V]): CondExp[T] = {
      val cond = buildExp(CondBuilder.of[V])
      wrap(dsl.forall(cond))
    }
  }

  implicit def asIterableOps[T, C[x] <: IterableOnce[x], V](builder: CondBuilder[T, C[V]]): IterableOps[T, C, V] = {
    new IterableOps[T, C, V](builder.wrap)
  }

}

final class CondBuilder[T, U](private val lens: NamedLens[T, U]) extends AnyVal {

  private def wrap(cond: CondExp[U]): CondExp[T] =
    FreeApplicative.lift(ExpAlg.Select[T, U, Boolean](lens, cond))

  def at[V](selector: NamedLens[T, U] => NamedLens[T, V]): CondBuilder[T, V] = new CondBuilder(selector(lens))

  def in(set: Set[U]): CondExp[T] = wrap(dsl.in(set))

  def within(window: Window[U]): CondExp[T] = wrap(dsl.within(window))

  def >(min: U)(implicit ord: Ordering[U]): CondExp[T] = within(Window.greaterThan(min))
  def >=(min: U)(implicit ord: Ordering[U]): CondExp[T] = within(Window.greaterThanOrEqual(min))
  def <(max: U)(implicit ord: Ordering[U]): CondExp[T] = within(Window.lessThan(max))
  def <=(max: U)(implicit ord: Ordering[U]): CondExp[T] = within(Window.lessThanOrEqual(max))
  def ===(value: U)(implicit ord: Eq[U]): CondExp[T] = wrap(equalTo(value))
}

class WhereBuilder[T, U <: T : ClassTag : TypeTag] private[dsl] (factTypeSet: FactTypeSet[U]) {

  def whereAnyFact(buildExp: CondBuilder[TypedFact[U], TypedFact[U]] => CondExp[TypedFact[U]]): TerminalFactsExp = {
    collectFacts {
      whereAny(TypedFact.lens[U], buildExp)
    }
  }

  def whereAnyFactValue(buildExp: CondBuilder[TypedFact[U], U] => CondExp[TypedFact[U]]): TerminalFactsExp = {
    collectFacts {
      whereAny(TypedFact.value[U], buildExp)
    }
  }

  private def whereAny[V](
    lens: NamedLens[TypedFact[U], V],
    buildExp: CondBuilder[TypedFact[U], V] => CondExp[TypedFact[U]],
  ): ExpAlg[FactsOfType[U], TypedResultSet[U]] = {
    ExpAlg.Exists[FactsOfType[U], TypedFact[U], TypedResultSet[U]](
      _.toList,
      buildExp(new CondBuilder(lens)),
      found => TypedResultSet.fromNel(found),
      _ => NoFactsMatch(),
    )
  }

  def whereEveryFact(buildExp: CondBuilder[TypedFact[U], TypedFact[U]] => CondExp[TypedFact[U]]): TerminalFactsExp = {
    collectFacts {
      whereEvery(TypedFact.lens[U], buildExp)
    }
  }

  def whereEveryFactValue(buildExp: CondBuilder[TypedFact[U], U] => CondExp[TypedFact[U]]): TerminalFactsExp = {
    collectFacts {
      whereEvery(TypedFact.value[U], buildExp)
    }
  }

  private def collectFacts(exp: ExpAlg[FactsOfType[U], TypedResultSet[U]]): TerminalFactsExp = {
    FreeApplicative.lift {
      ExpAlg.Collect[Facts, FactsOfType[U], ResultSet](
        s"Fact[${typeNameOf[U]}]",
        facts => NonEmptyList.fromList(facts.collect(factTypeSet.collector)),
        FreeApplicative.lift(exp).map(rs => rs: ResultSet),
        _ => NoFactsMatch(),
      )
    }
  }

  private def whereEvery[V](
    lens: NamedLens[TypedFact[U], V],
    buildExp: CondBuilder[TypedFact[U], V] => CondExp[TypedFact[U]],
  ): ExpAlg[FactsOfType[U], TypedResultSet[U]] = {
    ExpAlg.ForAll[FactsOfType[U], TypedFact[U], TypedResultSet[U]](
      _.toList,
      buildExp(new CondBuilder(lens)),
      _ => NoFactsMatch(),
      TypedFactsMatch(_),
    )
  }
}
