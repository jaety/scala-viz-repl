package jae.measure

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object NameFromInvocation {

  def ha : String = macro nameFromInvocationImpl
  def nameFromInvocationImpl(c: Context): c.Expr[String] = {
    import c.universe._
    c.Expr[String](q""""hello world"""")
    /*
    c.enclosingClass.collect {
      case ValDef(_, name, _, rhs)
        if rhs.pos == c.macroApplication.pos => c.literal(foo(name.decoded))
    }.headOption.getOrElse(
      c.abort(c.enclosingPosition, "Not a valid application.")
    )
    */
  }
}
