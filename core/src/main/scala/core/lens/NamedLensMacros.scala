package com.rallyhealth.vapors

package core.lens

import scala.annotation.tailrec
import scala.reflect.macros.blackbox

object NamedLensMacros {

  def selectImpl[A : c.WeakTypeTag, B : c.WeakTypeTag, C : c.WeakTypeTag](
    c: blackbox.Context,
  )(
    getter: c.Expr[B => C],
  ): c.Expr[NamedLens[A, C]] = {
    import c.universe._
    val selectChain = getter.tree match {
      case Function(_, rhs) => rhs
    }
    val fieldNames = selectTermNameList(c)(selectChain).map(JavaBeanCompat.unbeanify)
    val fieldNameLiterals = c.Expr[List[String]](q"$fieldNames")
    val dataPathToAppend = reify {
      DataPath(fieldNameLiterals.splice.map(DataPath.Field))
    }
    val lensToAppend = reify {
      NamedLens[B, C](dataPathToAppend.splice, getter.splice)
    }
    reify {
      c.prefix.splice.asInstanceOf[NamedLens.Selector[A, B]].lens.andThen(lensToAppend.splice)
    }
  }

  def selectTermNameList(c: blackbox.Context)(tree: c.mirror.universe.Tree): List[String] = {
    import c.universe._
    @tailrec def loop(
      remaining: Tree,
      fields: List[String],
    ): List[String] = remaining match {
      // once we have figured out the target of all of the select operations,
      // ignore the name (probably anonymous anyway) and return the field names
      case Ident(_) => fields
      // strip parameterless method application for Java compat
      case Apply(sub: Select, Nil) => loop(sub, fields)
      // recursive case: selects the term from the result of the given expression
      case Select(init, TermName(last)) => loop(init, last :: fields)
      // if the expression does more than just select or apply zero parameter methods, then stop
      case _ => c.abort(remaining.pos, "Can only extract term names from a chain of vals or parameterless methods")
    }
    loop(tree, Nil)
  }

}

object JavaBeanCompat {

  // originally this contained both "is" and "get", but I think x.isEmpty reads better than x.empty
  private final val GETTER_PREFIXES = Set("get")

  def unbeanify(name: String): String = {
    for (prefix <- GETTER_PREFIXES) {
      if (name.startsWith(prefix) && name.length > prefix.length) {
        val firstChar = name.charAt(prefix.length)
        if (firstChar.isUpper) {
          val restOfName = name.substring(prefix.length + 1)
          return s"${firstChar.toLower}$restOfName"
        }
      }
    }
    name
  }
}
