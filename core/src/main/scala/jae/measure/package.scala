package jae

/**
  * Created by jaety on 12/29/16.
  */
package object measure {
  implicit class MeasureMath[T](val a: Op[T,Double]) extends AnyVal {
    def +(b: Op[T,Double]) : Op[T,Double] = a zip b andThen (_ + _)
    def -(b: Op[T,Double]) : Op[T,Double] = a zip b andThen (_ - _)
    def *(b: Op[T,Double]) : Op[T,Double] = a zip b andThen (_ * _)
    def /(b: Op[T,Double]) : Op[T,Double] = a zip b andThen (_ / _)

    def >(b: Op[T,Double]) : Op[T,Boolean]= a zip b andThen (_ > _)
    def <(b: Op[T,Double]) : Op[T,Boolean]= a zip b andThen (_ < _)
    def >=(b: Op[T,Double]) : Op[T,Boolean]= a zip b andThen (_ >= _)
    def <=(b: Op[T,Double]) : Op[T,Boolean]= a zip b andThen (_ <= _)
  }

  def func[A,B](sources: Op[_,_]*)(op: Op[A,B]) = new Func[A,B](sources:_*) {
    lazy val expr = op
  }
  def func[A,B](nme: String, sources: Op[_,_]*)(op: Op[A,B]) = new Func[A,B](sources:_*) {
    lazy val expr = op
    override def tpe = nme
  }


  def prettyPrint(root: Op[_,_], depth: Int=0) {
    val indent = "  "*depth
    println(s"$indent${root.tpe} ${root.domain}")
    for { src <- root.sources } { prettyPrint(src, depth+1) }
  }

  implicit class Crossable[X](val xs: Traversable[X]) extends AnyVal {
    def cross[Y](ys: Traversable[Y]) : Seq[(X,Y)] = (for (x <- xs; y <- ys) yield (x,y)).toSeq
  }

  def standardPF[A,B](domain: Seq[A], f: A => B) = new PartialFunction[A,B] {
    override def isDefinedAt(x: A): Boolean = domain.contains(x)
    override def apply(v1: A): B = f(v1)
  }
  def standardPF[A,B](pf: PartialFunction[A,B]) = pf

}
