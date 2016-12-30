package jae.data

import java.io.File

import jae.domain.{CountryInfo, Country}
import org.json4s._
import org.json4s.native.JsonMethods._
import scala.io.Source

/**
  * Created by jaety on 12/30/16.
  */
object CountrySource {
  def countries = download

  // **********************
  // Private

  private case class RawData(name: String, `alpha-3`: String, `country-code`: String)
  private val cachefile = new File(cachedir, this.getClass.getCanonicalName)
  private val source = "https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/slim-3/slim-3.json"
  private def download = {
    implicit val formats = DefaultFormats
    val content = if (cachefile.exists()) Source.fromFile(cachefile) else Source.fromURL(source)
    val raw : Seq[RawData] = for {
      JArray(arr) <- parse(content.mkString)
      item <- arr
    } yield {
      item.extract[RawData]
    }
    raw.map(r => CountryInfo(Country(r.`alpha-3`), r.`alpha-3`, r.name))
  }

}
