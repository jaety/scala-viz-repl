package jae.measure

import jae.measure.Op.RichSeqOp

import scala.reflect.runtime.universe._

/**
  * Op[A,B] is an operation that produces a Measure[A,B] // TODO: Clean up the vocabulary
  *   Source[A,B] : Initial production of M[A,B]
  *   OrElse[A,B] : Merging Domains of two ops
  *   AndThen[A,B,C] : Transform M[A,B] to M[A,C]
  *   ZipN[A,R1...RN]: M[A,(R1...RN)]
  *
  **/





trait Op[A,+B] {

  def domain: Seq[A]

  def orElse[B1 >: B](that: Op[A,B1])(implicit domainTag: TypeTag[A], rangeTag: TypeTag[B1]) : Op[A,B1] = OrElse(this, that)
  def andThen[C](k: B => C)(implicit domainTag: TypeTag[A], rangeTag: TypeTag[C]) : Op[A,C] = AndThen(this, k)
  def partition[A2](k: A => A2)(implicit domainTag: TypeTag[A2], rangeTagA: TypeTag[A], rangeTagB: TypeTag[B]) : Op[A2,Seq[(A,B)]] = Partition[A2,A,B](this, k)

  def project[A2, C](k: A => A2, r: Seq[B] => C)(implicit domainTag: TypeTag[A2], rangeTag: TypeTag[C]) = Partition(this,k) andThen (s => r(s.map(_._2)))

  def asFunc : PartialFunction[A,B]
  def eval(a: A) = asFunc(a)

  def sources : Seq[Op[_,_]]

}
object Op {
  implicit class RichSeqOp[K,B](val op: Op[K, Seq[(_,B)]]) extends AnyVal {
    def reduce[R](f: Seq[B] => R)(implicit domainTag: TypeTag[K], rangeTag: TypeTag[R]) = op andThen (v => f(v.map(_._2)))
  }
}

case class Source[A,B](val domain: Seq[A])(f: A => B)
                      (implicit val domainTag: TypeTag[A], implicit val rangeTag: TypeTag[B]) extends Op[A,B] {
  override def toString = s"${getClass.getSimpleName}(${domainTag.tpe}, ${rangeTag.tpe}"
  def sources = Seq.empty
  def asFunc = new PartialFunction[A,B] {
    override def isDefinedAt(x: A): Boolean = domain.contains(x)
    override def apply(v1: A): B = f(v1)
  }
}

case class OrElse[A,B,B1 >: B](f1: Op[A,B], f2: Op[A,B1])
                              (implicit val domainTag: TypeTag[A], implicit val rangeTag: TypeTag[B1]) extends Op[A,B1] {
  override def toString = s"${getClass.getSimpleName}(${domainTag.tpe}, ${rangeTag.tpe})"
  def domain : Seq[A] = (f1.domain ++ f2.domain).distinct
  def sources = Seq(f1, f2)
  def asFunc = f1.asFunc orElse f2.asFunc
}

case class AndThen[A, B, C](op: Op[A,B], k: B=>C)
                            (implicit val domainTag: TypeTag[A], implicit val rangeTag: TypeTag[C]) extends Op[A,C] {
  override def toString = s"${getClass.getSimpleName}(${domainTag.tpe}, ${rangeTag.tpe})"
  def domain = op.domain
  def sources = Seq(op)
  def asFunc = op.asFunc andThen k
}

case class Partition[A2, A, +B](op: Op[A,B], k: A => A2)
                               (implicit val domainTag: TypeTag[A2], val rangeTagA: TypeTag[A], val rangeTagB: TypeTag[B]) extends Op[A2, Seq[(A,B)]] {
  override def toString = s"${getClass.getSimpleName}(${domainTag.tpe}, (${rangeTagA.tpe}, ${rangeTagB.tpe}))"
  lazy val domain = op.domain.map(k).distinct
  def sources = Seq(op)
  def asFunc = new PartialFunction[A2, Seq[(A,B)]] {
    override def isDefinedAt(x: A2): Boolean = domain.contains(x)
    override def apply(v1: A2): Seq[(A, B)] = {
      val x = op.domain.filter(k(_) == v1)
      x.map(a => (a,op.asFunc(a)))
    }
  }
}


object Take2 {
  /*
    m1 andThen m2
    // m2.source(a) => m1
    m1 orElse m2
    // m2.source(a) => m1 or m2 depending
    // but at this point I'll have lost typing for m1 or m2

    andThen : [A,B] => [A,C]
    orElse  : [A,B],[A,B] => [A,B]
    project : [A,A2,B] => [A2,B]

   */


  // Functions are operators that wrap potentially complicated expressions. They give nesting in the graph
  trait Func[A,B] extends Op[A,B] {
    def expr : Op[A,B]
    override def domain: Seq[A] = expr.domain
    override def asFunc: PartialFunction[A, B] = expr.asFunc
  }
  class DoStuff[T](m1: Source[T, Double], m2: Source[T,Double]) extends Func[T,Double] {
    val expr = m1 orElse m2 andThen (_ + 3.0)
    override def sources: Seq[Op[_, _]] = Seq(m1, m2)
  }

  def doStuff[T](m1: Source[T,Double], m2: Source[T,Double]) = m1 orElse m2 andThen (_ + 3.0)

  class genericF[K](m1: Source[Int, Double], m2: Source[Int, Double]) {
    val union = m1 orElse m2
    val plus3 = union andThen (_ + 3.0)
    val grouped = plus3 partition (_ < 5.0)
    val result = new RichSeqOp(grouped) reduce (_.sum)
  }

  def prettyPrint(root: Op[_,_], depth: Int = 0): Unit = {
    val line = root match {
      case Partition(op, f) => "Partition"
      case AndThen(op, f) => "AndThen"
      case OrElse(op1, op2) => "OrElse"
      case Source(domain) => "Measure"
    }
    // println(s"$line ${root.domainType} ${root.rangeType}")
  }

  def main(args: Array[String]): Unit = {
    val a = Source(1 to 10)(_ => 1.0)
    val b = Source(11 to 20)(_ => 2.0)
    val c = a orElse b andThen (_ + 3.0)
    val d = c partition (x => if (x < 5.0) 1 else 2)
    val e = d andThen (_.map(_._2).sum)

    println(c)
    println(c.sources)
    val f = c.asFunc
    println((1 to 20).map(i => (i,f(i))))
    println(d)
    println(d.domain)
    val f2 = d.asFunc
    println(1 to 2 map f2)
    println(1 to 2 map e.asFunc)

    val x = new genericF(a,b)
    println(Seq(true, false).map(x.result.asFunc))

    println()
    prettyPrint(x.result)
  }
}

