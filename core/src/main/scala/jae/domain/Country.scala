package jae.domain

/**
  * Created by jaety on 12/30/16.
  */
case class Country(val id: String) extends AnyVal // id == ISO3

trait CountryKeyed { def id: Country }
trait CountrySource[T <: CountryKeyed] { def countries : Seq[T]}

