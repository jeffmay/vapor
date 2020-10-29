package com.rallyhealth.vapors.factfilter.data

import cats.Order
import com.rallyhealth.vapors.core.data.{NamedLens, ValidDataPathKey}

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

/**
  * Type information required to construct a [[Fact]].
  */
trait FactType[T] {

  /**
    * The unique name for this fact type.
    */
  def name: String

  /**
    * The unique type name plus type information.
    */
  final lazy val fullName: String = s"'$name' as ${tt.tpe}"

  protected[vapors] implicit val ct: ClassTag[T]
  protected[vapors] implicit val tt: TypeTag[T]

  /**
    * Validates the value is the correct type, but packages it as an [[Fact]] to avoid
    * issues with the compiler attempting to find the LUB of the [[TypedFact]] type parameter using
    * a wildcard.
    *
    * If you need the specific type of fact for some reason, you can use the standard [[TypedFact]]
    * factory method and supply this fact type as the first parameter.
    */
  def apply(value: T): TypedFact[T] = TypedFact(this, value)

  /**
    * Safely cast the given fact to this type.
    */
  def cast(fact: Fact): Option[TypedFact[T]] = {
    fact match {
      // Justification: This checks validity of the FactType and safely casts the fact value using a class tag
      case f @ TypedFact(ft, _: T) if ft.tt.tpe <:< this.tt.tpe =>
        Some(f.asInstanceOf[TypedFact[T]])
      case f: Fact if f.typeInfo.tt.tpe <:< this.tt.tpe =>
        f.value match {
          case v: T => Some(TypedFact[T](f.typeInfo.asInstanceOf[FactType[T]], v))
          case _ => None
        }
      case _ => None
    }
  }

  /**
    * Defines FactType equality as "both types have the same name and compiled type."
    */
  override final def equals(o: Any): Boolean = o match {
    case that: FactType[_] =>
      this.fullName == that.fullName && this.tt.tpe =:= that.tt.tpe
    case _ => false
  }

  override final def hashCode: Int = this.fullName.hashCode()

  def productPrefix: String = "CustomFactType"

  override def toString: String = s"$productPrefix($fullName)"
}

object FactType {

  def apply[T : ClassTag : TypeTag](name: String): FactType[T] = Simple(name)

  private final case class Simple[T : ClassTag : TypeTag](name: String) extends FactType[T] {
    override protected[vapors] val ct: ClassTag[T] = implicitly
    override protected[vapors] val tt: TypeTag[T] = implicitly
    override def productPrefix: String = "FactType"
    override def productElementName(n: Int): String = n match {
      case 0 => "name"
      case _ => super.productElementName(n)
    }
  }

  implicit class ToLens[T](private val ft: FactType[T]) extends AnyVal {
    def lens: NamedLens[TypedFact[T], T] = TypedFact.value[T]
  }

  implicit def orderingByName[T]: Ordering[FactType[T]] = Ordering.by(_.fullName)

  implicit def orderByName[T]: Order[FactType[T]] = {
    import cats.instances.string._
    Order.by(_.fullName)
  }

  implicit def validDataPathKey[T]: ValidDataPathKey[FactType[T]] = _.fullName
}
