package jae.domain

case class Year(val id: Int) extends AnyVal with Wrapped with Ordered[Year] {
  override def compare(that: Year): Int = this.id.compare(that.id)
}
