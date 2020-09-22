package com.rallyhealth.vapors.core.dsl

import cats.data.NonEmptyList
import cats.free.FreeApplicative
import com.rallyhealth.vapors.core.algebra._
import com.rallyhealth.vapors.core.data._

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

final class FactValuesExpBuilder[T >: U, U : ClassTag : TypeTag, V] private[dsl] (
  factTypeSet: FactTypeSet[U],
  factLens: NamedLens[Fact[U], V],
) {

  private def selectFactValuesWhere(exp: ExpAlg[Facts[U], ResultSet[T]]): TerminalFactsExp[T] = {
    FreeApplicative.lift {
      ExpCollect[Facts[T], Facts[U], ResultSet[T]](
        s"Fact[${typeNameOf[U]}]",
        facts => NonEmptyList.fromList(facts.collect(factTypeSet.matchAsPartial[U])),
        FreeApplicative.lift(exp),
        FactsMatch(_),
      )
    }
  }

  def whereAllValues(condExp: CondExp[V]): TerminalFactsExp[T] = {
    selectFactValuesWhere {
      ExpForAll[Facts[U], Fact[U], ResultSet[T]](
        _.toList,
        FreeApplicative.lift {
          ExpSelectField[Fact[U], V, Boolean](factLens, condExp)
        },
        FactsMatch(_),
        _ => NoFactsMatch(),
      )
    }
  }

  def whereAnyValue(condExp: CondExp[V]): TerminalFactsExp[T] = {
    selectFactValuesWhere {
      ExpExists[Facts[U], Fact[U], ResultSet[T]](
        _.toList,
        FreeApplicative.lift {
          ExpSelectField[Fact[U], V, Boolean](factLens, condExp)
        },
        FactsMatch(_),
        _ => NoFactsMatch(),
      )
    }
  }
}
