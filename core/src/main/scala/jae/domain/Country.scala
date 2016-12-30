package jae.domain

/**
  * Created by jaety on 12/30/16.
  */
case class Country(val id: String) extends AnyVal
case class CountryInfo(country: Country, iso3: String, longName: String)
