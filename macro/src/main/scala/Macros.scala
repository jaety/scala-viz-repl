package jae.measure

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macros {

  def enclosingName: String = macro enclosingNameImpl
  def enclosingNameImpl(c: Context): c.Expr[String] = {
    import c.universe._
    val s = Literal(Constant(c.internal.enclosingOwner.fullName))
    c.Expr[String](s)
  }
}
