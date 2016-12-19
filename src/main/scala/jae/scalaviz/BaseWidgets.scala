package jae.scalaviz

/**
  * Created by jaety on 12/18/16.
  */

trait VizItem

trait VizComponent extends VizItem {
  def state: Any
}

trait VizGroup extends VizItem {
  def content: Seq[VizItem]
}

trait BaseWidgets {
  case class Row(content: Seq[VizItem]) extends VizGroup
  case class Col(content: Seq[VizItem]) extends VizGroup
  case class Stack(content: Seq[VizItem]) extends VizGroup

  case class Text(text: String) extends VizComponent {
    def state = Map("text" -> text)
  }
  case class LineChart(x: Seq[Double], ys: Seq[Seq[Double]]) extends VizComponent {
    def state = Map("traces" -> ys.map(y => Map("x" -> x, "y" -> y, "type" -> "scatter")))
  }

}
