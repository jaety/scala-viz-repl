package jae.measure

import java.io.File

import com.github.tototoshi.csv.CSVReader
import jae.data.CountrySource
import plotly._, element._, layout._
import plotly.Plotly._

/**
  * Created by jaety on 12/29/16.
  */
trait Wrapped extends Any {
  def id : Any
  override def toString = id.toString
}

case class Year(val id: Int) extends AnyVal with Wrapped
case class Country(val id: String) extends AnyVal with Wrapped
case class PopItem(year: Year, country: Country)

object DataWrangling {
  val dataDir = new File("data")

  /**
    * http://databank.worldbank.org/data/reports.aspx?source=2&series=SP.POP.TOTL&country=#
    * http://api.worldbank.org/countries/all/indicators/SP.POP.TOTL?per_page=1000&page=1
 *
    * @return
    */
  def loadPopulationData = {
    val input = new File(dataDir, "155db8f2-cf2c-4466-89d3-43db350af9d7_Data.csv")
    val reader= CSVReader.open(input)
    val lines = reader.all()
    val dates = lines.head.drop(4).map(s => Year(s.take(4).toInt))
    val body  = lines.tail.take(dates.length)
    val values : Map[PopItem, Double] = (for {
        line <- body
        strippedLine = line.drop(5).dropRight(1)
        country = Country(line(3))
        idx <- dates.indices
      } yield {
          PopItem(dates(idx),country) -> strippedLine.map(_.toDouble).applyOrElse(idx, (_:Int) => Double.NaN)
      }).toMap
    val (countries, data) = (body.map(s => Country(s(3))), body.map(_.drop(5)))
    Source(dates cross countries map PopItem.tupled)(values)
  }

  def plot[T](op: Op[T,Seq[Double]]) = {
    val traces = for { d <- op.domain } yield {
      Scatter(op.domain.indices, op(d), name=d.toString)
    }
    traces.plot()
  }

  def main(args: Array[String]): Unit = {
    val x = loadPopulationData
    val y = x.collect(_.country)
    println(CountrySource.countries.mkString("\n"))
//    plot(y)
  }
}
