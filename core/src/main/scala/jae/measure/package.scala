package jae

import scala.reflect.runtime.universe._

/**
  * Created by jaety on 12/29/16.
  */
package object measure {

  implicit class MeasureMath[T](val a: Op[T,Double]) extends AnyVal {
    def +(b: Op[T,Double]) : Op[T,Double] = func("+", a, b)(a zip b andThen (_ + _))
    def +(b: Double) : Op[T,Double] = a + a.fill(b)

    def -(b: Op[T,Double]) : Op[T,Double] = func("-", a, b)(a zip b andThen (_ - _))
    def -(b: Double) : Op[T,Double] = a + a.fill(b)

    def *(b: Op[T,Double]) : Op[T,Double] = func("*", a, b)(a zip b andThen (_ * _))
    def *(b: Double) : Op[T,Double] = a * a.fill(b)

    def /(b: Op[T,Double]) : Op[T,Double] = func("/", a, b)(a zip b andThen (_ / _))
    def /(b: Double) : Op[T,Double] = a / a.fill(b)

    def >(b: Op[T,Double]) : Op[T,Boolean]= func(">", a, b)(a zip b andThen (_ > _))
    def >(b: Double) : Op[T,Boolean] = a > a.fill(b)

    def <(b: Op[T,Double]) : Op[T,Boolean]= func("<", a, b)(a zip b andThen (_ < _))
    def <(b: Double) : Op[T,Boolean] = a < a.fill(b)

    def >=(b: Op[T,Double]) : Op[T,Boolean]=func(">=",a, b)(a zip b andThen (_ >= _))
    def >=(b: Double) : Op[T,Boolean] = a >= a.fill(b)

    def <=(b: Op[T,Double]) : Op[T,Boolean]=func("<=",a, b)(a zip b andThen (_ <= _))
    def <=(b: Double) : Op[T,Boolean] = a <= a.fill(b)

    def aggregate[U](grouper: T=>U, f: Seq[Double] => Double = _.sum)(implicit r: TypeTag[T]) : Op[U,Double] = func("aggregate",a)(a groupValuesBy grouper map f)
  }

  def func[A,B](sources: Op[_,_]*)(op: Op[A,B])(implicit rangeTag: TypeTag[B]) = new Func[A, B](sources: _*) {
    lazy val expr = op
  }
  def func[A,B](nme: String, sources: Op[_,_]*)(op: Op[A,B])(implicit rangeTag: TypeTag[B]) = new Func[A,B](sources:_*) {
    lazy val expr = op
    override def name = Some(nme)
  }

  def JoinAll[A,B](op: Seq[Op[A,B]])(implicit t: TypeTag[B]) = func("JoinAll", op:_*)(op.tail.foldLeft(op.head)(_ orElse _))

  def prettyPrint(root: Op[_,_], depth: Int=0) {
    val indent = "  "*depth
    root match {
//      case m: Named[_,_] => print(m.realName)
      case _ => println(s"$indent${root.toString}")
    }
    for { src <- root.sources } { prettyPrint(src, depth+1) }
  }

  implicit class Crossable[X](val xs: Traversable[X]) extends AnyVal {
    def cross[Y](ys: Traversable[Y]) : Seq[(X,Y)] = (for (x <- xs; y <- ys) yield (x,y)).toSeq
  }

  private[measure] def standardPF[A,B](domain: Seq[A], f: A => B) = new PartialFunction[A,B] {
    override def isDefinedAt(x: A): Boolean = domain.contains(x)
    override def apply(v1: A): B = f(v1)
  }
  private[measure] def standardPF[A,B](pf: PartialFunction[A,B]) = pf

}
