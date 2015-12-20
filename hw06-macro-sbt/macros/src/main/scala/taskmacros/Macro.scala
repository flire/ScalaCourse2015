package taskmacros

/**
 * Created by flire on 14.12.15.
 */

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object Macros {
  def getType(e: => Any): String = macro gettype_impl

  def gettype_impl(c: blackbox.Context)(e: c.Expr[Any]): c.Expr[String] = {
    import c.universe._
    c.Expr[String](q"${e.actualType.toString()}")
  }
}

