package net.flatmap.commonmark

/**
  * @see http://spec.commonmark.org/0.26/#characters-and-lines
  *
  * A line is a sequence of zero or more characters other than newline
  * (U+000A) or carriage return (U+000D), followed by a line ending or by the
  * end of file.
  *
  * A line ending is a newline (U+000A), a carriage return (U+000D) not
  * followed by a newline, or a carriage return and a following newline.
  *
  * A line containing no characters, or a line containing only spaces (U+0020)
  * or tabs (U+0009), is called a blank line.
  */

sealed trait Line {
  val number: Int
  val content: String
  val isBlank: Boolean = true
  def trimmed = content.trim()
  def unindentBy(n: Int) = this
  def indentation: Int = 0
}

object Line {
  def apply(n: Int, s: String): Line =
    if (s.forall(CharacterClasses.IndentationCharacter)) Blank(n,s)
    else NonBlank(n,s)

  case class Blank(number: Int, content: String) extends Line

  case class NonBlank(number: Int, content: String) extends Line {
    override val isBlank: Boolean = false

    override def indentation: Int =
      Tabs.replaceMeaningfulTabs(this.content).prefixLength(CharacterClasses.Space)

    override def unindentBy(n: Int): Line = {
      val ln = Tabs.replaceMeaningfulTabs(this.content).drop(n)
      NonBlank(number,ln)
    }
  }

  object ThematicBreak {
    def unapply(l: Line): Option[Blocks.ThematicBreak.type] =if (
      l.indentation <= 3 &&
        (l.content.count(_ == '-') >= 3 && l.content.forall(ch => CharacterClasses.WhitespaceCharacter(ch) || ch == '-')) ||
        (l.content.count(_ == '_') >= 3 && l.content.forall(ch => CharacterClasses.WhitespaceCharacter(ch) || ch == '_')) ||
        (l.content.count(_ == '*') >= 3 && l.content.forall(ch => CharacterClasses.WhitespaceCharacter(ch) || ch == '*'))
    ) Some(Blocks.ThematicBreak) else None
  }

  object ATXHeading {
    val atxHeadingSuffix = " #+ *$".r

    def unapply(l: Line): Option[Blocks.Heading] = if (
      l.indentation <= 3
    ) {
      val (h,r) = l.trimmed.span(_ == '#')
      val level = h.length
      if (level > 0 && level <= 6 && r.startsWith(" ") || r.isEmpty) {
        Some(Blocks.Heading(level,atxHeadingSuffix.replaceFirstIn(r,"").trim))
      } else None
    } else None
  }

  object SetextHeadingUnderline {
    def unapply(l: Line): Option[Int] =  {
      if (l.indentation <= 3) {
        if (l.content.trim.forall(_ == '=')) Some(1)
        else if (l.content.trim.forall(_ == '-')) Some(2)
        else None
      } else None
    }
  }

  object CodeLine {
    def unapply(l: Line): Option[String] = l match {
      case Blank(_,_) => Some("")
      case other if other.indentation >= 4 =>
        Some(other.unindentBy(4).content)
      case _ => None
    }
  }

  object OpeningCodeFence {
    def unapply(l: Line): Option[(Int,Char,Int,Option[String])] = {
      if (l.indentation <= 3) {
        val trimmed = l.trimmed
        if (trimmed.startsWith("```")) {
          val (prefix,rest) = trimmed.span(_ == '`')
          val info = rest.trim match { case "" => None; case other => Some(other) }
          if (!rest.contains('`')) Some(l.indentation,'`',prefix.length,info) else None
        } else if (trimmed.startsWith("~~~")) {
          val (prefix,rest) = trimmed.span(_ == '~')
          val info = rest.trim match { case "" => None; case other => Some(other) }
          if (!rest.contains('`')) Some(l.indentation,'~',prefix.length,info) else None
        } else None
      } else None
    }
  }

  object ClosingCodeFence {
    def unapply(l: Line): Option[(Char,Int)] =
      if (l.indentation <= 3) {
        var trimmed = l.trimmed
        if (trimmed.length >= 3 && (trimmed.forall(_ == '`') || trimmed.forall(_ == '~')))
          Some(trimmed.head,trimmed.length)
        else None
      } else None
  }

  object HTMLBlock {
    val htmlBlockRegex1 = "^(<script|<pre|<style)(>|\\s|$)".r
    val htmlBlockRegex1Close = "(</script>|</pre>|</style>)".r

    val htmlBlockRegex2 = "^<!--".r
    val htmlBlockRegex2Close = "-->".r

    val htmlBlockRegex3 = "^<\\?".r
    val htmlBlockRegex3Close = "\\?>".r

    val htmlBlockRegex4 = "^<![A-Z]".r
    val htmlBlockRegex4Close = ">".r

    val htmlBlockRegex5 = "^<!\\[CDATA\\[".r
    val htmlBlockRegex5Close = "\\]\\]>".r

    val htmlBlockRegex6 = ("^</?(address|article|aside|base|basefont|blockquote|" +
      "body|caption|center|col|colgroup|dd|details|dialog|dir|div|dl|dt|fieldset|" +
      "figcaption|figure|footer|form|frame|frameset|h1|head|header|hr|html|iframe|" +
      "legend|li|link|main|menu|menuitem|meta|nav|noframes|ol|optgroup|option|p|" +
      "param|section|source|summary|table|tbody|td|tfoot|th|thead|title|tr|track|ul)(\\s+|$|>|/>|)").r

    //val htmlBlockRegex7 = ("^<[a-zA-Z][a-zA-Z0-9\\-]*")

    def unapply(l: Line): Option[(String,Line => Boolean)] =
      if (htmlBlockRegex1.findFirstMatchIn(l.content).nonEmpty)
        Some(l.content, (x: Line) => htmlBlockRegex1Close.findFirstMatchIn(x.content).nonEmpty)
      else if (htmlBlockRegex2.findFirstMatchIn(l.content).nonEmpty)
        Some(l.content, (x: Line) => htmlBlockRegex2Close.findFirstMatchIn(x.content).nonEmpty)
      else if (htmlBlockRegex3.findFirstMatchIn(l.content).nonEmpty)
        Some(l.content, (x: Line) => htmlBlockRegex3Close.findFirstMatchIn(x.content).nonEmpty)
      else if (htmlBlockRegex4.findFirstMatchIn(l.content).nonEmpty)
        Some(l.content, (x: Line) => htmlBlockRegex4Close.findFirstMatchIn(x.content).nonEmpty)
      else if (htmlBlockRegex5.findFirstMatchIn(l.content).nonEmpty)
        Some(l.content, (x: Line) => htmlBlockRegex5Close.findFirstMatchIn(x.content).nonEmpty)
      else if (htmlBlockRegex6.findFirstMatchIn(l.content).nonEmpty)
        Some(l.content, (x: Line) => x.isBlank)
      else None
  }

  object BlockQuoteMarker {
    val blockQuoteRegex = "^ {0,3}> ?(.*)$".r

    def unapply(l: Line): Option[Line] =
      blockQuoteRegex.findFirstMatchIn(Tabs.replaceMeaningfulTabs(l.content)).map(x =>
        Line(l.number, x.group(1)))
  }

  object UnorderedListMarker {
    val bulletListMarkerRegex = "^( {0,3}(-|\\+|\\*) {1,4})(.*)$".r
    def unapply(l: Line): Option[(Char,Line,Line => Option[Line])] =
      bulletListMarkerRegex.findFirstMatchIn(Tabs.replaceMeaningfulTabs(l.content)).map { m =>
        val witdh = m.group(0).length
        (m.group(2).head,Line(l.number,m.group(3)),
          (l: Line) => l match {
            case blank: Blank => Some(blank)
            case l: NonBlank =>
              if (l.indentation >= witdh)
                Some(l.unindentBy(witdh))
              else None
          })
      }
  }
}

object Lines {
  def apply(input: String): Iterator[Line] = apply(input.lines)

  /**
    *
    * @param input
    * @return
    */
  def apply(input: Iterator[String]): Iterator[Line] =
    input.zipWithIndex.map{ case (x,y) => Line(y,x) }
}