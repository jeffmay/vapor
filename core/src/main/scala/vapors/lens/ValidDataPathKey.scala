package com.rallyhealth

package vapors.lens

import cats.Contravariant
import shapeless.Nat
import shapeless.ops.nat.ToInt

/**
  * Defines a type that can be used as the key in a [[Map]].
  *
  * @note you should consider extending [[Equals]] to make sure that your type is
  *       actually safe to use as a key.
  */
trait ValidDataPathKey[K] {

  def stringify(key: K): String
}

object ValidDataPathKey {

  def apply[K](implicit valid: ValidDataPathKey[K]): ValidDataPathKey[K] = valid

  def instance[K](f: K => String): ValidDataPathKey[K] = k => f(k)

  implicit object Instances extends Contravariant[ValidDataPathKey] {
    override def contramap[A, B](fa: ValidDataPathKey[A])(f: B => A): ValidDataPathKey[B] = { key =>
      fa.stringify(f(key))
    }
  }

  implicit def atNatIdx[N <: Nat : ToInt]: ValidDataPathKey[N] = { _ =>
    ToInt[N].apply().toString
  }

  implicit val string: ValidDataPathKey[String] = identity[String]

  implicit val int: ValidDataPathKey[Int] = _.toString

  implicit val long: ValidDataPathKey[Long] = _.toString

}
