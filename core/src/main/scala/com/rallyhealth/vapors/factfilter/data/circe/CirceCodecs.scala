package com.rallyhealth.vapors.factfilter.data.circe

import com.rallyhealth.vapors.factfilter.data.{Fact, FactType, FactTypeSet, ResultSet, TypedFact}
import io.circe.{Encoder, Json}
import io.circe.syntax._

object CirceCodecs extends CirceCodecs

trait CirceCodecs {

  private val encodeAnyFactType: Encoder[FactType[Any]] = Encoder[String].contramap(_.fullName)

  implicit def encodeFactType[T]: Encoder[FactType[T]] = encodeAnyFactType.asInstanceOf[Encoder[FactType[T]]]

  private val encodeAnyFactTypeSet: Encoder[FactTypeSet[Any]] =
    Encoder.encodeList(encodeAnyFactType).contramap(_.typeList.toList)

  implicit def encodeFactTypeSet[T]: Encoder[FactTypeSet[T]] =
    encodeAnyFactTypeSet.asInstanceOf[Encoder[FactTypeSet[T]]]

  def encodeUntypedFact(fact: Fact)(implicit encodeValue: Encoder[fact.Value]): Json = {
    Json.obj(
      "factName" -> fact.typeInfo.name.asJson,
      "factType" -> fact.typeInfo.tt.tpe.toString.asJson,
      "value" -> fact.value.asJson,
    )
  }

  implicit def encodeTypedFact[T : Encoder]: Encoder[TypedFact[T]] = Encoder.instance { fact =>
    encodeUntypedFact(fact)
  }

  val encodeFactWithToString: Encoder[Fact] = Encoder.instance { fact =>
    encodeUntypedFact(fact)(_.toString.asJson)
  }

  val encodeEvidenceWithToString: Encoder[ResultSet] =
    Encoder.encodeList(encodeFactWithToString).contramapArray(_.matchingFacts)

}
