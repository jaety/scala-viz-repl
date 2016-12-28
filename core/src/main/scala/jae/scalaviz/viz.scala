package jae.scalaviz

import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets




trait Easy extends BaseWidgets {
  def writer : Writer
  def $(content: VizItem) = {
    writer.writeFile(content)
    content
  }
  def chart(x: Seq[Double], y: Seq[Double]) : VizItem = $(Row(Seq(LineChart(x, Seq(y)))))
}

object vizlib extends Easy {
  val writer = new Writer("web/data.json")
}

class Writer(val filename: String) {
  def writeFile(content: VizItem) = {
    implicit val formats = Serialization.formats(NoTypeHints) ++ GoldenLayout.serializers
    val json = write(Map("content" -> Seq(content)))
    Files.write(Paths.get(filename), json.getBytes(StandardCharsets.UTF_8))
  }
}

//object test {
//  def main(args: Array[String]) : Unit = {
//    import layouts._
//    val page = Row(Seq(
//      Text("hello world. Ha"),
//      LineChart(Seq(1.0, 2.0, 3.0), Seq(Seq(1.0, 3.0, 2.0), Seq(3.0, 1.0, 2.0)))
//    ))
//    writeDataFile(page, "web/data.js")
//  }
//}
