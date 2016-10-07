package net.flatmap.commonmark

case class Range(from: Int, to: Int)

sealed trait Block
sealed trait LeafBlock extends Block

case object ThematicBreak extends LeafBlock
case class Heading(level: Int, inline: String) extends LeafBlock
case class Paragraph(raw: String) extends LeafBlock
