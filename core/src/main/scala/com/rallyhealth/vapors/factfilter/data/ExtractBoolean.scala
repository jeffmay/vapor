package com.rallyhealth.vapors.factfilter.data

/**
  * Special case of [[ExtractValue]] that extracts a [[Boolean]] value.
  */
trait ExtractBoolean[-T] extends ExtractValue[T, Boolean] {

  override final def extractValue(obj: T): Boolean = isTrue(obj)

  def isTrue(value: T): Boolean
}

object ExtractBoolean {
  @inline final def apply[T](implicit instance: ExtractBoolean[T]): ExtractBoolean[T] = instance

  implicit object boolean extends ExtractBoolean[Boolean] {
    override def isTrue(value: Boolean): Boolean = value
  }
}
