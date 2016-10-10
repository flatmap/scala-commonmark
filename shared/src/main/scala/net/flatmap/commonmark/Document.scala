package net.flatmap.commonmark

case class Document(blocks: Seq[Block])

/**
  * Created by martin on 10.10.16.
  */
object Document {
  def apply(input: String): Document = apply(Lines(input))

  def apply(lines: Iterator[Line]): Document =
    Document((lines).foldLeft(ParserState.empty) {
    case (state,line) => state.line(line)
  }.blocks)
}
