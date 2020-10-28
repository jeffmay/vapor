package com.rallyhealth.vapors.factfilter.dsl

import cats.Eq
import cats.free.FreeApplicative
import com.rallyhealth.vapors.core.algebra._
import com.rallyhealth.vapors.core.data._
import com.rallyhealth.vapors.core.util.ReflectUtils.typeNameOf
import com.rallyhealth.vapors.factfilter.data._
import com.rallyhealth.vapors.factfilter.dsl

import scala.collection.Factory
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

//  implicit def asFilterBuilder[C[x] <: IterableOnce[x], U](
//    builder: CondBuilder[C[U], C[U]],
//  )(implicit
//    f: Factory[U, C[U]],
//  ): FilterBuilder[C, U, U, Boolean] = {
//    new FilterBuilder[C, U, U, Boolean](
//      NamedLens.id[U],
//      identity[IterableOnce[U]],
//      _.iterator.to(f),
//      _.nonEmpty,
//      _.isEmpty,
//    )
//  }

}

final class FactFilterBuilder[U]
  extends FilterBuilder[List, TypedFact[U], TypedFact[U], TypedResultSet[U]](
    NamedLens.id[TypedFact[U]],
    _.toList,
    _.iterator.toList,
    TypedResultSet(_),
    _ => NoFactsMatch(),
  ) {

  def value: FilterBuilder[List, TypedFact[U], U, TypedResultSet[U]] = at(_.select(_.value))
}

class FilterBuilder[C[_], T, U, A] private[dsl] (
  protected val itemLens: NamedLens[T, U],
  protected val toIterable: C[T] => IterableOnce[T],
  protected val fromIterable: IterableOnce[T] => C[T],
  protected val withMatching: List[T] => A,
  protected val ifNoMatch: List[T] => A,
) {

  def at[V](selector: NamedLens[T, U] => NamedLens[T, V]): FilterBuilder[C, T, V, A] = {
    new FilterBuilder(
      selector(itemLens),
      toIterable,
      fromIterable,
      withMatching,
      ifNoMatch,
    )
  }

  def exists(buildExp: CondBuilder[T, U] => CondExp[T]): Exp[C[T], A] = {
    FreeApplicative.lift {
      ExpAlg.Exists[C[T], T, A](
        toIterable,
        buildExp(new CondBuilder(itemLens)),
        nel => withMatching(nel.toList),
        all => ifNoMatch(toIterable(all).iterator.toList),
      )
    }
  }

  def forall(buildExp: CondBuilder[T, U] => CondExp[T]): Exp[C[T], A] = {
    FreeApplicative.lift {
      ExpAlg.ForAll[C[T], T, A](
        toIterable,
        buildExp(new CondBuilder(itemLens)),
        nel => ifNoMatch(nel.toList),
        all => withMatching(toIterable(all).iterator.toList),
      )
    }
  }
}

final class CondBuilder[T, U](private val lens: NamedLens[T, U]) extends AnyVal {

  private def wrap(cond: CondExp[U]): CondExp[T] =
    FreeApplicative.lift(ExpAlg.Select[T, U, Boolean](lens, cond))

  def at[V](selector: NamedLens[T, U] => NamedLens[T, V]): CondBuilder[T, V] = new CondBuilder(selector(lens))

  def withValueAt[V](
    selector: NamedLens[T, U] => NamedLens[T, V],
  )(
    buildExp: CondBuilder[T, V] => CondExp[T],
  ): CondExp[T] = {
    buildExp(at(selector))
  }

  def in(set: Set[U]): CondExp[T] = wrap(dsl.in(set))

  def within(window: Window[U]): CondExp[T] = wrap(dsl.within(window))

  def >(min: U)(implicit ord: Ordering[U]): CondExp[T] = within(Window.greaterThan(min))
  def >=(min: U)(implicit ord: Ordering[U]): CondExp[T] = within(Window.greaterThanOrEqual(min))
  def <(max: U)(implicit ord: Ordering[U]): CondExp[T] = within(Window.lessThan(max))
  def <=(max: U)(implicit ord: Ordering[U]): CondExp[T] = within(Window.lessThanOrEqual(max))
  def ===(value: U)(implicit ord: Eq[U]): CondExp[T] = wrap(equalTo(value))
}

class WhereBuilder[T, U <: T : ClassTag : TypeTag] private[dsl] (factTypeSet: FactTypeSet[U]) {

  def where(buildExp: FactFilterBuilder[U] => Exp[List[TypedFact[U]], TypedResultSet[U]]): TerminalFactsExp = {
    val exp = buildExp(new FactFilterBuilder[U])
    collectFacts(exp)
  }

  @deprecated("Use where(_.value.exists(...)) instead.", "0.2.0")
  def whereAnyFact(buildExp: CondBuilder[TypedFact[U], TypedFact[U]] => CondExp[TypedFact[U]]): TerminalFactsExp = {
    collectFacts {
      FreeApplicative.lift {
        whereAny(TypedFact.lens[U], buildExp)
      }
    }
  }

  @deprecated("Use where(_.exists(...)) instead.", "0.2.0")
  def whereAnyFactValue(buildExp: CondBuilder[TypedFact[U], U] => CondExp[TypedFact[U]]): TerminalFactsExp = {
    collectFacts {
      FreeApplicative.lift {
        whereAny(TypedFact.value[U], buildExp)
      }
    }
  }

  private def whereAny[V](
    lens: NamedLens[TypedFact[U], V],
    buildExp: CondBuilder[TypedFact[U], V] => CondExp[TypedFact[U]],
  ): ExpAlg[List[TypedFact[U]], TypedResultSet[U]] = {
    ExpAlg.Exists[List[TypedFact[U]], TypedFact[U], TypedResultSet[U]](
      _.toList,
      buildExp(new CondBuilder(lens)),
      TypedResultSet.fromNel,
      _ => NoFactsMatch(),
    )
  }

  @deprecated("Use where(_.forall(...)) instead.", "0.2.0")
  def whereEveryFact(buildExp: CondBuilder[TypedFact[U], TypedFact[U]] => CondExp[TypedFact[U]]): TerminalFactsExp = {
    collectFacts {
      FreeApplicative.lift {
        whereEvery(TypedFact.lens[U], buildExp)
      }
    }
  }

  @deprecated("Use where(_.value.forall(...)) instead.", "0.2.0")
  def whereEveryFactValue(buildExp: CondBuilder[TypedFact[U], U] => CondExp[TypedFact[U]]): TerminalFactsExp = {
    collectFacts {
      FreeApplicative.lift {
        whereEvery(TypedFact.value[U], buildExp)
      }
    }
  }

  private def collectFacts(exp: Exp[List[TypedFact[U]], TypedResultSet[U]]): TerminalFactsExp = {
    FreeApplicative.lift {
      ExpAlg.Collect[Facts, List[TypedFact[U]], ResultSet](
        s"Fact[${typeNameOf[U]}]",
        facts => {
          val matching = facts.collect(factTypeSet.collector)
          Option.when(matching.nonEmpty)(matching)
        },
        exp.map(rs => rs: ResultSet),
        _ => NoFactsMatch(),
      )
    }
  }

  private def whereEvery[V](
    lens: NamedLens[TypedFact[U], V],
    buildExp: CondBuilder[TypedFact[U], V] => CondExp[TypedFact[U]],
  ): ExpAlg[List[TypedFact[U]], TypedResultSet[U]] = {
    ExpAlg.ForAll[List[TypedFact[U]], TypedFact[U], TypedResultSet[U]](
      _.toList,
      buildExp(new CondBuilder(lens)),
      _ => NoFactsMatch(),
      TypedResultSet(_),
    )
  }
}
