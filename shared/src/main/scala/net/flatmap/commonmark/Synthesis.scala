package net.flatmap.commonmark

/**
  * Created by martin on 07/10/2016.
  */
object Synthesis {
  def html(i: Seq[Inline])(implicit dummyImplicit: DummyImplicit): String = i
    .map(html).mkString

  def html(i: Inline): String = i match {
    case Inlines.Plain(t) => HTMLEntities.replaceAll(t)
    case Inlines.Escape(ch) => ch.toString
    case Inlines.Code(c) => s"<code>${HTMLEntities.replaceAll(c)}</code>"
  }

  def html(d: Document): String = html(d.blocks)

  def html(b: Seq[Block]): String = b.map(html).mkString("\n")

  def html(b: Block): String = b match {
    case Blocks.Code(None,c) => s"<pre><code>${c}</code></pre>"
    case Blocks.Code(Some(i),c) =>
      val l = "language-" + i.takeWhile(!_.isWhitespace)
      s"""<pre><code class="$l">${c}</code></pre>"""
    case Blocks.ThematicBreak => "<hr />"
    case Blocks.Heading(l,s) => s"<h$l>${html(Inlines(s))}</h$l>"
    case Blocks.Paragraph(x) => s"<p>${html(Inlines(x))}</p>"
    case Blocks.HTML(c) => c
    case Blocks.BlockQuote(blocks) => s"<blockquote>\n${
      blocks.map(b => html(b) + '\n').mkString
    }</blockquote>"
    case Blocks.UnorderedList(_,items) => s"<ul>\n${
      html(items)
    }\n</ul>"
    case Blocks.ListItem(_,blocks) => s"<li>\n${
      html(blocks)
    }\n</li>"

  }
}
