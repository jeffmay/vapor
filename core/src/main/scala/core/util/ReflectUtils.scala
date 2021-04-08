package com.rallyhealth.vapors

package core.util

import scala.reflect.runtime.universe.{typeOf, TypeTag}

object ReflectUtils {

  /**
    * Returns the full class name or type name but with the package path removed.
    *
    * Specifically, it removes all portions of the path that start with a lowercase letter.
    * This is to work better with enumeration types and inner classes. It is a simple heuristic
    * (albeit, an arbitrary one), so if you want the type name to include the surrounding object
    * name, that object needs to start with a capital letter.
    */
  def typeNameOf[T : TypeTag]: String = typeOf[T].toString.split('.').dropWhile(_.charAt(0).isLower).mkString(".")
}
