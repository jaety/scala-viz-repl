package jae.measure

/**
  * Measure[A,B] encodes an enumerable PartialFunction  // TODO: Clean up the vocabulary
  * Op[A,B] is a measure with provenance
  * Source[A,B] : Initial production of M[A,B]
  * OrElse[A,B] : Merging Domains of two ops
  * AndThen[A,B,C] : Transform M[A,B] to M[A,C]
  * ZipN[A,R1...RN]: M[A,(R1...RN)]
  *
  * TODO:
  * * naming: top level expressions should, somehow, get names. Probably macro
  * * memoization
  *
  * ## naming
  *
  *
  *
  **/
trait Measure[A, +B] {
  def domain: Seq[A]
  def toPF: PartialFunction[A,B]
  def apply(a:A) = toPF(a)
}

trait Op[A, +B] extends Measure[A, B] {
  def sources: Seq[Op[_, _]]

  def orElse[B1 >: B](that: Op[A, B1]) : OrElse[A,B1] = OrElse(this, that)
  def andThen[C](f: B => C) : Op[A,C] = AndThen(this, f)
  def zip[C, B1 >: B](that: Op[A,C]) : Zip[A,B1,C] = Zip(this, that)
  def zip3[C1,C2, B1 >: B](that1: Op[A,C1], that2: Op[A,C2]) : Zip3[A,B1,C1,C2] = Zip3(this, that1, that2)
  def collectItems[A2](collector: A => A2) : Op[A2, Seq[(A,B)]] = Collect(this, collector)
  def collect[A2](collector: A => A2) : Op[A2, Seq[B]] = this collectItems collector andThen (items => items.map(_._2))
  def fillFrom[A2,B2](op: Op[A2,B2])(f: A => A2) : Op[A,B2] = FillFrom(this, op, f)

  def onto[A2](domain: Seq[A2])(spreader: A2 => A) : Op[A2, B] = Onto(this, domain, spreader)
  def onto[A2](that: Op[A2,_])(spreader: A2 => A) : Op[A2, B] = Onto(this, that.domain, spreader)

  // Utilities: not fundamental
  def tpe = getClass.getSimpleName
  def name: Option[String] = None

  //
  override def toString = s"$tpe(${domain.head}..${domain.last} [${domain.length}])"

}

case class Source[A,B](domain: Seq[A])(f: A => B) extends Op[A,B] {
  lazy val sources = Seq.empty
  lazy val toPF = standardPF(domain, f)
}
object Source {
  def apply[A,B,C](domainA: Seq[A], domainB: Seq[B])(f: (A,B) => C) : Source[(A,B),C]= {
    val items = for {a <- domainA; b <- domainB} yield (a,b)
    Source(items)(f.tupled)
  }
  def fromList[A,B](items: Seq[(A,B)]) : Source[A,B] = Source(items.map(_._1))(items.toMap)
}

case class OrElse[A,B](f1: Op[A,B], f2: Op[A,B]) extends Op[A,B] {
  lazy val sources : Seq[Op[_, _]] = Seq(f1, f2)

  lazy val domain: Seq[A] = (f1.domain ++ f2.domain).distinct

  lazy val toPF = standardPF(f1.toPF orElse f2.toPF)
}

case class AndThen[A,B,C](op: Op[A,B], f: B => C) extends Op[A,C] {
  override def sources: Seq[Op[_, _]] = Seq(op)

  override def domain: Seq[A] = op.domain

  def toPF = standardPF(op.toPF andThen f)
}

case class Zip[A,B1,B2](f1: Op[A,B1], f2: Op[A,B2]) extends Op[A,(B1,B2)] {
  override def sources: Seq[Op[_, _]] = Seq(f1, f2)
  override def domain: Seq[A] = f1.domain intersect f2.domain
  def toPF = standardPF(domain, (a:A) => (f1(a), f2(a)))
  def andThen[C](f: (B1,B2) => C) : Op[A,C] = this andThen f.tupled
}
case class Zip3[A,B1,B2,B3](f1: Op[A,B1], f2: Op[A,B2], f3: Op[A,B3]) extends Op[A, (B1,B2,B3)] {
  override def sources: Seq[Op[_, _]] = Seq(f1,f2,f3)

  override def domain: Seq[A] = Seq(f1,f2,f3).map(_.domain).reduceLeft(_ intersect _)

  def toPF = standardPF(domain, (x: A) => (f1(x), f2(x), f3(x)))

  def andThen[C](f: (B1,B2,B3) => C) : Op[A,C] = this andThen f.tupled
}

case class Collect[A2, A, B](op: Op[A,B], collector: A => A2) extends Op[A2, Seq[(A,B)]] {
  override def sources: Seq[Op[_, _]] = Seq(op)

  override def domain: Seq[A2] = op.domain.map(collector).distinct

  override def toPF = standardPF(domain, (x:A2) => op.domain.filter(item => collector(item) == x).map(k => (k,op(k))))
}

case class Onto[A2, A, B](op: Op[A,B], override val domain: Seq[A2], spreader: A2 => A) extends Op[A2, B] {
  override def sources: Seq[Op[_,_]] = Seq(op)
  override def toPF = standardPF(domain, spreader andThen op.toPF)
}

case class FillFrom[A,B,A2,B2](domainSrc: Op[A,B], valueSrc: Op[A2,B2], mapper: A => A2) extends Op[A,B2] {
  override def sources: Seq[Op[_, _]] = Seq(valueSrc)
  override def domain: Seq[A] = domainSrc.domain
  override def toPF = standardPF(domain, mapper andThen valueSrc.toPF)
}

case class Filter[A,B](op: Op[A,B], pred: A => Boolean) extends Op[A,B] {
  override def sources: Seq[Op[_, _]] = Seq(op)
  override def domain: Seq[A] = op.domain.filter(pred)
  override def toPF = standardPF(domain, op.toPF)
}

abstract class Func[A,+B](val sources: Op[_,_]*) extends Op[A,B] {
  def expr: Op[A,B]
  lazy val domain: Seq[A] = expr.domain
  lazy val toPF = standardPF(expr.toPF)
  override def toString = expr.toString
}


  // ---------------------------
  // Examples
  // ---------------------------



  class MyLogic {
    val a = Source(1 to 20)(_ => 1.0)
    val b = Source(1 to 10, 'a' to 'c')((_,_) => 2.0)
    val a2= a.onto(b) (_._1)
    val b2= b.onto(a) ((_, 'a'))
    val b3= b collect (_._1) andThen (_.sum)

    val c = a + b3
    val d = a + b2 + b3
    // val reduced = c.filter(c < d)
    // val e = where (c < d)(1.0)
  }

object Take3 {

  def add[T](m1: Op[T,Double], m2: Op[T,Double]) = func("add", m1,m2) {
    m1 zip m2 andThen (_ + _)
  }

  def dist[T](m1: Op[T,Seq[Double]]) = func(m1) {
    val items = for {a <- m1.domain; b <- m1.domain} yield (a,b)
    Source(items)(pair => {
      // TODO: Bring in basic operations
      Math.sqrt((for { (a,b) <- m1(pair._1) zip m1(pair._2)} yield { Math.pow(a-b, 2) }).sum)
    })
  }

  def main(args: Array[String]): Unit = {
    val a = Source(1 to 10)(_ => 1.0)
    val b = Source(11 to 20)(_ => 3.0)
    val c = a orElse b
    val d = add(a,a) orElse c
    // prettyPrint(d)

    val logic = new MyLogic
    // val c = add(a, b)
    println(d.domain.map(x => (x, d.toPF(x))))
//    println(d.toPF.stashCount)
//    d.toPF.reset()
//    println(d.toPF.stashCount)

  }
}

