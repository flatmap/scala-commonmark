package net.flatmap.commonmark2

sealed trait Inline

object Inline {
  case class Code(text: String) extends Inline
  case object LineBreak extends Inline
}
