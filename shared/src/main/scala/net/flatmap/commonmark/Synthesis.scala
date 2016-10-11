package net.flatmap.commonmark

/**
  * Created by martin on 07/10/2016.
  */
object Synthesis {
  def escape(s: String) = s
    .replace("&","&amp;")
    .replace("\"","&quot;")
    .replace("<","&lt;")
    .replace(">","&gt;")
    .replace("\\#","#")

  def html(d: Document): String = html(d.blocks)

  def html(b: Seq[Block]): String = b.map(html).mkString("\n")

  def html(b: Block): String = b match {
    case Blocks.Code(None,c) => s"<pre><code>${escape(c)}</code></pre>"
    case Blocks.Code(Some(i),c) =>
      val l = "language-" + i.takeWhile(!_.isWhitespace)
      s"""<pre><code class="$l">${escape(c)}</code></pre>"""
    case Blocks.ThematicBreak => "<hr />"
    case Blocks.Heading(l,s) => s"<h$l>${escape(s)}</h$l>"
    case Blocks.Paragraph(x) => s"<p>${escape(x)}</p>"
    case Blocks.HTML(c) => c
    case Blocks.BlockQuote(blocks) => s"<blockquote>\n${
      blocks.map(b => html(b) + '\n').mkString
    }</blockquote>"
  }
}
