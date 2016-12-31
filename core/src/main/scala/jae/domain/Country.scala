package jae.domain

/**
  * Created by jaety on 12/30/16.
  */
trait Wrapped extends Any {
  def id : Any
  override def toString = id.toString
}

case class Country(val id: String) extends AnyVal with Wrapped

trait CountryKeyed { def id: Country }
trait CountrySource[T <: CountryKeyed] { def countries : Seq[T]}

