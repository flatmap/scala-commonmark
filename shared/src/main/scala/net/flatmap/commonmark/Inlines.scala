package net.flatmap.commonmark

sealed trait Inline

object Inlines {
  def apply(s: String): Seq[Inline] = {
    codeSpans(s)
  }

  val backtickString = "`+".r
  def codeSpans(s: String): Seq[Inline] = {
    backtickString.findFirstMatchIn(s) match {
      case Some(m) =>
        val l = m.matched.length
        val (before,rest) = s.splitAt(m.start)
        val after = rest.drop(l)
        backtickString.findAllMatchIn(after).find(_.matched.length == l) match {
          case Some(m2) =>
            val (code,rest) = after.splitAt(m2.start)
            val after2 = rest.drop(m2.matched.length)
            Vector(Plain(before),Code(code.trim.replaceAll("[ \t]*[ \t\\n][\t]*", " ")), Plain(after2))
          case None =>
            Vector(Plain(s))
        }
      case None => Vector(Plain(s))
    }
  }

  case class Code(s: String) extends Inline
  case class Emphasis(s: String) extends Inline
  case class Plain(s: String) extends Inline
  case class Escape(ch: Char) extends Inline
}
