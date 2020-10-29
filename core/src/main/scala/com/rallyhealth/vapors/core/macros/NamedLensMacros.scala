package com.rallyhealth.vapors.core.macros

import com.rallyhealth.vapors.core.data.NamedLens

import scala.reflect.macros.blackbox

object NamedLensMacros {

  def selectImpl[A : c.WeakTypeTag, B : c.WeakTypeTag, C : c.WeakTypeTag](
    c: blackbox.Context,
  )(
    getter: c.Expr[B => C],
  ): c.Expr[NamedLens[A, C]] = {
    import c.universe._
    // TODO: Handle nested fields better
    // TODO: Better error message for invalid function types and call patterns
    // TODO: Use quasiquotes for everything?
//    println(s"SHOWING THIS PREFIX RAW: ${showRaw(c.prefix)}")
//    println(s"SHOWING THIS PREFIX CODE: ${showCode(c.prefix.tree)}")
//    println(s"SHOWING GETTER RAW: ${showRaw(getter)}")
//    val selectorCls = weakTypeTag[NamedLens.Selector[A, B]]
//    val q"$selector($lens)" = c.prefix.tree
//    if (!(selector.tpe =:= selectorCls.tpe)) {
//      c.error(c.enclosingPosition, "Can only call .select from NamedLens")
//    }
//    c.Expr[NamedLens[A, C]](q"$lens.field($fieldName, $getter)")
    val fieldName = getter.tree match {
      case q"(..$_) => $_.${TermName(value)}" => value
      case q"(..$_) => $_.${TermName(getMethod)}()" => getMethod
    }
    val fieldNameExp = c.Expr[String](q"$fieldName")
    reify {
      c.prefix.splice.asInstanceOf[NamedLens.Selector[A, B]].lens.field(fieldNameExp.splice, getter.splice)
    }
  }

}
