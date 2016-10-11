package net.flatmap.commonmark2

sealed trait Block {
}

object Block {
  sealed trait Leaf extends Block

  sealed trait Container extends Block {
    val blocks: Seq[Block]
  }

  case class Document(blocks: Seq[Block], references: Map[String,String])
    extends Container

  case class ThematicBreak() extends Leaf
}
