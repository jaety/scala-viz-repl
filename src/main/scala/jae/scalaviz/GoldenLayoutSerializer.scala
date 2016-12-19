package jae.scalaviz

import net.liftweb.json._

object GoldenLayout extends BaseWidgets {
  // TODO: How can I combine these for Row, Col, Stack in the presence of type erasure (partial function match)
  private object GroupSerializer extends Serializer[VizGroup] {
    def typ(g: VizGroup) = g match {
      case _:Row => "row"
      case _:Col => "column"
      case _:Stack => "stack"
    }
    def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), VizGroup] = {
      throw new MappingException(s"group deserialization not implemented")
    }
    def serialize(implicit format: Formats) : PartialFunction[Any, JValue] = {
      case g: VizGroup => JObject(
        JField("type", JString(typ(g))) :: JField("content", Extraction.decompose(g.content)) :: Nil
      )
    }
  }

  private object ComponentSerializer extends Serializer[VizComponent] {
    def name(c: VizComponent) = c.getClass.getSimpleName

    def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), VizComponent] = {
      throw new MappingException(s"component deserialization not implemented")
    }
    def serialize(implicit format: Formats) : PartialFunction[Any, JValue] = {
      case c: VizComponent => JObject(
        JField("type", JString("component")) ::
          JField("componentName", JString(name(c))) ::
          JField("componentState", Extraction.decompose(c.state)) ::
          Nil
      )
    }
  }

  val serializers : Seq[Serializer[_]]= Seq(GroupSerializer, ComponentSerializer)
}

