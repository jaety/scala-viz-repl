package jae.measure

import scala.reflect.runtime.universe._

trait Measure[A, +B] {
  def domain: Seq[A]
  def toPF: PartialFunction[A,B]
  def apply(a:A) = toPF(a)
}

trait Op[A, B] extends Measure[A, B] {
  def sources: Seq[Op[_, _]]

  // M[A,B],M[A,B] => M[A,B]
  def orElse(that: Op[A, B])(implicit r: TypeTag[B]) : OrElse[A,B] = OrElse(this, that)

  // M[A,B] => M[A,C]
  def andThen[C](f: B => C)(implicit r: TypeTag[C])  : Op[A,C] = AndThen(this, f)
  def map[C](f: B => C)(implicit r: TypeTag[C]) = andThen(f)
  def mapItems[C](f: (A,B) => C)(implicit r: TypeTag[C])  : MapItems[A,C,B] = { MapItems(this, f) }
  def fill[C](c:C)(implicit r: TypeTag[C]) : Op[A,C] = func(c.toString)(this andThen (_ => c))
  def filter(f: A => Boolean)(implicit r: TypeTag[B]) : Filter[A,B] = Filter(this, f)
  // TODO: Filter Values

  // M[A,B],M[A,C] => M[A,(B,C)]
  def zip[C](that: Op[A,C])(implicit rB: TypeTag[B], rC: TypeTag[C])  : Zip[A,B,C] = Zip(this, that)
  // M[A,B],M[A,C],M[A,D] => M[A,(B,C,D)]
  def zip3[C1,C2](that1: Op[A,C1], that2: Op[A,C2])(implicit rB: TypeTag[B], rC: TypeTag[C1], rC2: TypeTag[C2]) : Zip3[A,B,C1,C2] = Zip3(this, that1, that2)

  // M[A,B] => M[A,Seq[(A,B)]]
  def groupBy[A2](collector: A => A2)(implicit ta: TypeTag[A], tb: TypeTag[B]) : Op[A2, Seq[(A,B)]] = Collect(this, collector)
  // M[A,B] => M[A,Seq[B]]
  def groupValuesBy[A2](collector: A => A2)(implicit ta: TypeTag[A], tb: TypeTag[B]) : Op[A2, Seq[B]] = this groupBy collector andThen (items => items.map(_._2))

  // M[A,B],M[C,D] => M[A,D]
  def fillFrom[A2,B2](op: Op[A2,B2])(f: A => A2)(implicit t: TypeTag[B2]) : Op[A,B2] = FillFrom(this, op, f)

  // Utilities: not fundamental
  def tpe = getClass.getSimpleName
  def name: Option[String] = None
  def values = domain.map(this.apply)

  def domainDesc = if (domain.length>1) s"${domain.head}..${domain.last} [${domain.length}]" else s"${domain.head}"
  override def toString= {
    val n = name.map(_ + " ").getOrElse("")
    s"$n$tpe($domainDesc)"
  }

}


trait Source[A,B] extends Op[A,B] with WithName[Source[A,B]]

object Source {
  private case class SourceImpl[A,B](domain: Seq[A], f: A => B, override val name: Option[String])(implicit rangeTag: TypeTag[B]) extends Source[A,B] with WithName[Source[A,B]] {
    override def tpe = "Source"
    override def toString = super.toString + s" [${rangeTag.tpe}]"
    lazy val sources = Seq.empty
    lazy val toPF = standardPF(domain, f)
    def withName(name: String) = SourceImpl(domain, f, Some(name))
  }

  def apply[A,B](domain: Seq[A])(f: A=>B)(implicit t: TypeTag[B]) : Source[A,B] = SourceImpl(domain,f, None)
  def apply[A,B](domain: Seq[A], name: Option[String])(f: A=>B)(implicit t: TypeTag[B]) : Source[A,B] = SourceImpl(domain,f,name)
  def apply[A,B,C](domainA: Seq[A], domainB: Seq[B])(f: (A,B) => C)(implicit t: TypeTag[B], t2: TypeTag[C]) : Source[(A,B),C]= {
    val items = for {a <- domainA; b <- domainB} yield (a,b)
    Source(items)(f.tupled)
  }
  def fromList[A,B](items: Seq[(A,B)], name: Option[String] = None)(implicit t: TypeTag[B]) : Source[A,B] = Source(items.map(_._1), name)(items.toMap)
}

case class OrElse[A,B](f1: Op[A,B], f2: Op[A,B], override val name: Option[String] = None)(implicit rangeTag: TypeTag[B]) extends Op[A,B] with WithName[OrElse[A,B]] {
  override def toString = super.toString + s" [${rangeTag.tpe}]"
  lazy val sources : Seq[Op[_, _]] = Seq(f1, f2)

  lazy val domain: Seq[A] = (f1.domain ++ f2.domain).distinct

  lazy val toPF = standardPF(f1.toPF orElse f2.toPF)

  def withName(name: String) = OrElse(f1,f2,Some(name))
}

case class AndThen[A,B,C](op: Op[A,B], f: B => C)(implicit rangeTag: TypeTag[C]) extends Op[A,C] {
  override def sources: Seq[Op[_, _]] = Seq(op)

  override def domain: Seq[A] = op.domain

  def toPF = standardPF(op.toPF andThen f)
}

case class MapItems[A,C,B](op: Op[A,B], f: (A,B) => C, override val name: Option[String] = None)
                          (implicit rangeTag: TypeTag[C]) extends Op[A,C] with WithName[MapItems[A,C,B]] {
  override def sources: Seq[Op[_,_]] = Seq(op)
  override def domain: Seq[A] = op.domain
  def toPF = standardPF(domain, (a:A) => f(a,op(a)))
  def withName(name: String) = { MapItems(op, f, Some(name)) }
}

case class Zip[A,B1,B2](f1: Op[A,B1], f2: Op[A,B2], override val name: Option[String] = None)
                       (implicit rangeTag1: TypeTag[B1], rangeTag2: TypeTag[B2]) extends Op[A,(B1,B2)] with WithName[Zip[A,B1,B2]] {
  override def sources: Seq[Op[_, _]] = Seq(f1, f2)
  override def domain: Seq[A] = f1.domain intersect f2.domain
  def toPF = standardPF(domain, (a:A) => (f1(a), f2(a)))
  def andThen[C](f: (B1,B2) => C)(implicit t: TypeTag[C]) : Op[A,C] = this andThen f.tupled
  def withName(name: String) = { Zip(f1, f2, Some(name))}
}
case class Zip3[A,B1,B2,B3](f1: Op[A,B1], f2: Op[A,B2], f3: Op[A,B3], override val name: Option[String] = None)
                           (implicit r1: TypeTag[B1], r2: TypeTag[B2], r3: TypeTag[B3]) extends Op[A, (B1,B2,B3)] with WithName[Zip3[A,B1,B2,B3]] {
  override def sources: Seq[Op[_, _]] = Seq(f1,f2,f3)
  override def domain: Seq[A] = Seq(f1,f2,f3).map(_.domain).reduceLeft(_ intersect _)
  def toPF = standardPF(domain, (x: A) => (f1(x), f2(x), f3(x)))
  def andThen[C](f: (B1,B2,B3) => C)(implicit t: TypeTag[C]) : Op[A,C] = this andThen f.tupled
  def withName(name: String) = { Zip3(f1, f2, f3, Some(name))}
}

case class Collect[A2, A, B](op: Op[A,B], collector: A => A2, override val name: Option[String] = None)
                            (implicit t: TypeTag[A], t2: TypeTag[B]) extends Op[A2, Seq[(A,B)]] with WithName[Collect[A2,A,B]]{
  override def sources: Seq[Op[_, _]] = Seq(op)
  override def domain: Seq[A2] = op.domain.map(collector).distinct
  override def toPF = standardPF(domain, (x:A2) => op.domain.filter(item => collector(item) == x).map(k => (k,op(k))))
  def withName(name: String) = { Collect(op, collector, Some(name)) }
}

case class FillFrom[A,B2,A2,B](domainSrc: Op[A,B], valueSrc: Op[A2,B2], mapper: A => A2, override val name: Option[String] = None)
                              (implicit t: TypeTag[B2]) extends Op[A,B2] with WithName[FillFrom[A,B2,A2,B]] {
  override def sources: Seq[Op[_, _]] = Seq(valueSrc)
  override def domain: Seq[A] = domainSrc.domain
  override def toPF = standardPF(domain, mapper andThen valueSrc.toPF)
  def withName(name: String) = FillFrom(domainSrc, valueSrc, mapper, Some(name))
}

case class Filter[A,B](op: Op[A,B], pred: A => Boolean, override val name: Option[String] = None)
                      (implicit t: TypeTag[B]) extends Op[A,B] with WithName[Filter[A,B]] {
  override def sources: Seq[Op[_, _]] = Seq(op)
  override def domain: Seq[A] = op.domain.filter(pred)
  override def toPF = standardPF(domain, op.toPF)
  def withName(name: String) = Filter(op, pred, Some(name))
}

abstract class Func[A,B](val sources: Op[_,_]*)(implicit rangeTag: TypeTag[B]) extends Op[A,B] {
  def expr: Op[A,B]
  lazy val domain: Seq[A] = expr.domain
  lazy val toPF = standardPF(expr.toPF)
  override def toString = { name.map(n => s"$n $domainDesc [${rangeTag.tpe}]").getOrElse(super.toString) }
}

case class Named[A,B](op: Op[A,B], realName: String)(implicit t: TypeTag[B]) extends Op[A,B] {
  override def sources: Seq[Op[_, _]] = op.sources
  override def domain: Seq[A] = op.domain
  override def toPF: PartialFunction[A, B] = op.toPF
  override def tpe = realName
}

  // ---------------------------
  // Examples
  // ---------------------------



  class MyLogic {
    val a = Source(1 to 20)(_ => 1.0)
    val b = Source(1 to 10, 'a' to 'c')((_,_) => 2.0)
    val a2= b.fillFrom(a)(_._1) // a.onto(b) (_._1)
    val b2= a.fillFrom(b) ((_, 'a'))
    val b3= b.aggregate(_._1, _.sum)

    val c = a + b3
    val d = a + b2 + b3
    // val reduced = c.filter(c < d)
    // val e = where (c < d)(1.0)
  }

object Take3 {

  def add[T](m1: Op[T,Double], m2: Op[T,Double]) = func("Add", m1,m2) {
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
    import Macros.{name => $}
    val a = $(Source(1 to 10)(_ => 1.0))
    val b = $(Source(11 to 20)(_ => 3.0))
    val c = $(a orElse b)
    val d = $( (a + a * 2.0) orElse c)
    // prettyPrint(d)

    val logic = new MyLogic
    prettyPrint(logic.d)
    // val c = add(a, b)
//    println(d.domain.map(x => (x, d.toPF(x))))
//    println(d.toPF.stashCount)
//    d.toPF.reset()
//    println(d.toPF.stashCount)

  }
}

