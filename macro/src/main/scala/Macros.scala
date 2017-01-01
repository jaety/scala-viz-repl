package jae.measure

import scala.io.Source
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

trait WithName[T] {
  def withName(name: String) : T
}

object Macros {

  def name[T <: WithName[T]](expr: T) : T = macro nameImpl[T]
  def nameImpl[T <: WithName[T]](c: Context)(expr: c.Expr[T]) : c.Expr[T] = {
    import c.universe._
    val s = Literal(Constant(c.internal.enclosingOwner.name.toString))
    c.Expr[T](q"$expr.withName($s)")
  }

  def enclosingName: String = macro enclosingNameImpl
  def enclosingNameImpl(c: Context): c.Expr[String] = {
    import c.universe._
    val s = Literal(Constant(c.internal.enclosingOwner.owner.owner.fullName))
    c.Expr[String](s)
  }

  def sourceCode[T](expr: => T): T = macro sourceCodeImpl[T]
  def sourceCodeImpl[T](c: Context)(expr: c.Expr[T]): c.Expr[T] = {
    import c.universe._
    val pos = expr.tree.pos
    val src = Literal(Constant(Source.fromFile(pos.source.path).slice(pos.start, pos.end).mkString))
    val t = q"""NamedFunction(${expr.tree}, $src )"""
    c.Expr(t)
    // c.Expr(Literal(Constant(s"${pos.source.path}: line ${pos.line}, column ${pos.column}, start ${pos.start}, end ${pos.end}, src ${src}")))
  }
}
