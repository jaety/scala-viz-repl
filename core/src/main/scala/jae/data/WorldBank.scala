package jae.data

import jae.domain.{Year, Country}
import jae.measure.Source
import org.json4s._
import org.json4s.native.JsonMethods._

import scala.annotation.tailrec

/**
  * Created by jaety on 12/30/16.
  */
object WorldBank {
  val baseUrl = "http://api.worldbank.org"
  implicit val formats = DefaultFormats

  @tailrec
  def retrieve(source: String, format: String="json", page: Int=1) : Seq[String] = {
    // TODO More robust URL stitching
    val page1 = getWithCaching(source + s"&format=$format&page=$page")
    val pageInfo = parse(page1)(0)
    val nPages = (pageInfo \ "pages").extract[Int]
    val currPage=(pageInfo \ "page").extract[Int]

    if (currPage == nPages) Seq(page1)
    else retrieve(source, page=page+1)
  }

  def indicator(country: Country, indicator: String) : Source[Country, Source[Year, Double]] = {
    val url = s"http://api.worldbank.org/countries/$country/indicators/$indicator?per_page=1000"
    val src = Source.fromList(retrieve(url).flatMap(page => {
      val content = parse(page)(1)
      for { item <- content.children } yield {
        // item.extract[ValueDatePair]
        val value = item \ "value" match {
          case JNull => Double.NaN
          case t: JString => t.extract[String].toDouble
          case _ => throw new IllegalArgumentException("Unexpected json content")
        }
        Year((item \ "date").extract[String].toInt) -> value
      }
    }).sortBy(_._1))
    Source.fromList(Seq(country -> src), Some(s"WorldBank $indicator"))
  }

  def populationTotals(country: Country) : Source[Country, Source[Year, Double]] = indicator(country, "SP.POP.TOTL")
}
