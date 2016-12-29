package jae.measure

class MemoizePF[-T, +R](f: PartialFunction[T,R]) extends PartialFunction[T,R] {
  import scala.collection.mutable
  private[this] val vals = mutable.Map.empty[T, R]
  private[this] val valid = mutable.Map.empty[T,Boolean]

  def apply(x: T): R = {
    if (vals.contains(x)) {
      vals(x)
    }
    else {
      val y = f(x)
      vals += ((x, y))
      y
    }
  }

  override def isDefinedAt(x: T): Boolean = {
    valid.get(x) match {
      case Some(b) => b
      case None =>
        val b = f.isDefinedAt(x)
        valid(x) = b
        b
    }
  }
}

object Memoize {
  def apply[T, R](f: PartialFunction[T,R]) = new MemoizePF(f)
}
