package net.flatmap.commonmark

/**
  * Created by martin on 07/10/2016.
  */
object Synthesis {
  def escape(s: String) = s
    .replace("\"","&quot;")
    .replace("&","&amp;")
    .replace("<","&lt;")
    .replace(">","&gt;")

  def html(b: Seq[Block]): String = b.map(html).mkString("\n")

  def html(b: Block): String = b match {
    case ThematicBreak => "<hr />"
    case Heading(l,s) => s"<h$l>$s</h$l>"
    case Paragraph(x) => s"<p>${escape(x)}</p>"
  }
}
