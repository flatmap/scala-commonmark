package net.flatmap.commonmark

case class Range(from: Int, to: Int)

sealed trait Block

object Blocks {

  sealed trait LeafBlock extends Block

  case object ThematicBreak extends LeafBlock

  case class Heading(level: Int, inline: String) extends LeafBlock

  object Heading {
    def apply(level: Int, lines: Seq[Line]): Heading = Heading(level,lines.map(_.trimmed).mkString("\n"))
  }

  case class Code(info: Option[String], content: String) extends LeafBlock {
    def addLine(s: String) = Code(info,content + s + "\n")
  }

  case class HTML(content: String) extends LeafBlock {
    def addLine(s: String) = HTML(content + "\n" + s)
  }

  case class Paragraph(inline: String) extends LeafBlock

  object Paragraph {
    def apply(lines: Seq[Line]): Paragraph = Paragraph(lines.map(_.trimmed).mkString("\n"))
  }

}