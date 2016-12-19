package jae.scalaviz

import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

object layouts {
  trait Item
  trait Component extends Item {
    def state: Any
  }
  trait Group extends Item {
    def content: Seq[Item]
  }
  case class Row(content: Seq[Item]) extends Group
  case class Col(content: Seq[Item]) extends Group
  case class Stack(content: Seq[Item]) extends Group

  case class Text(text: String) extends Component {
    def state = Map("text" -> text)
  }
  case class LineChart(x: Seq[Double], ys: Seq[Seq[Double]]) extends Component {
    def state = Map("traces" -> ys.map(y => Map("x" -> x, "y" -> y, "type" -> "scatter")))
  }

  object GoldenLayout {
    // TODO: How can I combine these for Row, Col, Stack in the presence of type erasure (partial function match)
    private object GroupSerializer extends Serializer[Group] {
      def typ(g: Group) = g match {
        case _:Row => "row"
        case _:Col => "column"
        case _:Stack => "stack"
      }
      def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), Group] = {
        throw new MappingException(s"group deserialization not implemented")
      }
      def serialize(implicit format: Formats) : PartialFunction[Any, JValue] = {
        case g: Group => JObject(
          JField("type", JString(typ(g))) :: JField("content", Extraction.decompose(g.content)) :: Nil
        )
      }
    }

    private object ComponentSerializer extends Serializer[Component] {
      def name(c: Component) = c.getClass.getSimpleName

      def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), Component] = {
        throw new MappingException(s"component deserialization not implemented")
      }
      def serialize(implicit format: Formats) : PartialFunction[Any, JValue] = {
        case c: Component => JObject(
          JField("type", JString("component")) ::
          JField("componentName", JString(name(c))) ::
          JField("componentState", Extraction.decompose(c.state)) ::
          Nil
        )
      }
    }

    val serializers : Seq[Serializer[_]]= Seq(GroupSerializer, ComponentSerializer)
  }

  def writeDataFile(content: layouts.Item, filename: String) = {
    implicit val formats = Serialization.formats(NoTypeHints) ++ layouts.GoldenLayout.serializers
    val json = write(Map("content" -> Seq(content)))
    val contents = s"var data = $json"
    Files.write(Paths.get(filename), contents.getBytes(StandardCharsets.UTF_8))
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
