package net.flatmap.commonmark2

sealed trait Line {
  val lineNumber: Int
  val firstColumn: Int
  val text: String
}

case class OriginalLine(lineNumber: Int, text: String) extends Line {
  val firstColumn = 0
}

case class LineView(firstColumn: Int, origin: OriginalLine) extends Line {
  val lineNumber = origin.lineNumber
  val text = origin.text.drop(firstColumn)
}

object Line {

}