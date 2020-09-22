package com.rallyhealth.vapors.core.dsl

import com.rallyhealth.vapors.core.data.{Fact, FactTypeSet, NamedLens}

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

final class TypedFactExpBuilder[T >: U, U : ClassTag : TypeTag] private[dsl] (factTypeSet: FactTypeSet[U]) {

  // TODO: These terminal expression methods can be optimized to avoid an unnecessary select expression

  def whereAnyFact(condExp: CondExp[Fact[U]]): TerminalFactsExp[T] = {
    new FactValuesExpBuilder[T, U, Fact[U]](factTypeSet, NamedLens.id[Fact[U]]).whereAnyValue(condExp)
  }

  def whereAnyValue(condExp: CondExp[U]): TerminalFactsExp[T] = {
    new FactValuesExpBuilder[T, U, U](factTypeSet, Fact.value[U]).whereAnyValue(condExp)
  }

  def whereAllFacts(condExp: CondExp[Fact[U]]): TerminalFactsExp[T] = {
    new FactValuesExpBuilder[T, U, Fact[U]](factTypeSet, NamedLens.id[Fact[U]]).whereAllValues(condExp)
  }

  def whereAllValues(condExp: CondExp[U]): TerminalFactsExp[T] = {
    new FactValuesExpBuilder[T, U, U](factTypeSet, Fact.value[U]).whereAllValues(condExp)
  }

  def withValuesAt[V](lens: NamedLens.Id[U] => NamedLens[U, V]): FactValuesExpBuilder[T, U, V] = {
    new FactValuesExpBuilder(factTypeSet, Fact.value[U].andThen(lens(NamedLens.id[U])))
  }
}
