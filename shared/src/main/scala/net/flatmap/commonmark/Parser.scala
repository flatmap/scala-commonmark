package net.flatmap.commonmark

import net.flatmap.commonmark.Line.{ATXHeading, OpeningCodeFence, SetextHeadingUnderline, ThematicBreak}

sealed trait ParserState {
  def line(l: Line): ParserState
  def blocks: Seq[Block]
}

object ParserState {
  def empty: ParserState = Clean(Vector.empty)

  case class Clean(blocks: Seq[Block]) extends ParserState {
    def line(l: Line): ParserState = l match {
      case BlankLine(_, _) => this
      case ThematicBreak(break) =>
        Clean(blocks :+ break)
      case ATXHeading(heading) =>
        Clean(blocks :+ heading)
      case Line.CodeLine(s) =>
        IndentedCode(blocks,Blocks.Code(None,s + "\n"))
      case OpeningCodeFence(indentation,char,width,info) =>
        FencedCode(blocks,indentation,char,width,Blocks.Code(info,""))
      case l@Line.HTMLBlock(html, matchEnd) =>
        if (matchEnd(l)) Clean(blocks :+ Blocks.HTML(l.content))
        else HTMLBlock(blocks,Blocks.HTML(html),matchEnd)
      case l =>
        Paragraph(blocks,Vector(l))
    }
  }

  case class Paragraph(previous: Seq[Block], uninterpreted: Seq[Line]) extends ParserState {
    def line(l: Line): ParserState = l match {
      case BlankLine(_,_) => Clean(blocks)
      case SetextHeadingUnderline(level) =>
        Clean(previous :+ Blocks.Heading(level,uninterpreted))
      case ThematicBreak(break) =>
        Clean(blocks :+ break)
      case ATXHeading(heading) =>
        Clean(blocks :+ heading)
      case l@Line.HTMLBlock(html, matchEnd) =>
        if (matchEnd(l)) Clean(blocks :+ Blocks.HTML(l.content))
        else HTMLBlock(blocks,Blocks.HTML(html),matchEnd)
      case OpeningCodeFence(indentation,char,width,info) =>
        FencedCode(blocks,indentation,char,width,Blocks.Code(info,""))
      case l =>
        Paragraph(previous, uninterpreted :+ l)
    }

    def blocks: Seq[Block] =
      if (uninterpreted.isEmpty) blocks
      else previous :+ Blocks.Paragraph(uninterpreted)
  }

  case class IndentedCode(previous: Seq[Block], code: Blocks.Code) extends ParserState {
    def line(l: Line): ParserState = l match {
      case Line.CodeLine(l) => IndentedCode(previous,code.addLine(l))
      case other => Clean(blocks).line(l)
    }
    def blocks = previous :+ code
  }

  case class FencedCode(previous: Seq[Block], indentation: Int, char: Char, width: Int, code: Blocks.Code) extends ParserState {
    def line(l: Line): ParserState = l match {
      case Line.ClosingCodeFence(c,width) if c == char && width >= this.width => Clean(blocks)
      case other =>
        val unindented = other.unindentBy(indentation).content
        FencedCode(previous, indentation, char, width,code.addLine(unindented))
    }

    def blocks = previous :+ code
  }

  case class HTMLBlock(previous: Seq[Block], html: Blocks.HTML, matchEnd: Line => Boolean) extends ParserState {
    def line(l: Line): ParserState = l match {
      case l: BlankLine if matchEnd(l) => Clean(blocks)
      case l: NonBlankLine if matchEnd(l) => Clean(previous :+ html.addLine(l.content))
      case other => HTMLBlock(previous,html.addLine(other.content),matchEnd)
    }
    def blocks = previous :+ html
  }
}

/**
  * Created by martin on 07/10/2016.

class CommonMark(val input: ParserInput) extends Parser {
  //var nodes = Map.empty[Block]

  //def positioned[T](r: Rule1[(Range => T)]): Rule1[T] = rule {
    //push(cursor) ~ r ~> ((from: Int, f: Range => T) => f(Range(from,cursor)))
  //}

  /**
    * A character is a Unicode code point. Although some code points (for
    * example, combining accents) do not correspond to characters in an
    * intuitive sense, all code points count as characters for purposes of
    * this spec.
    */
  val character: CharPredicate = CharPredicate.from(x => x != EOI)

  /**
    * A line is a sequence of zero or more characters other than newline
    * (U+000A) or carriage return (U+000D), followed by a line ending or by
    * the end of file.
    */
  def line: Rule0 = rule {
    zeroOrMore(noneOf("\n\r")) ~ (lineEnding | EOI)
  }

  /**
    * A line ending is a newline (U+000A), a carriage return (U+000D) not
    * followed by a newline, or a carriage return and a following newline.
    */
  def lineEnding: Rule0 = rule {
    '\n' | '\r' ~ '\n'.?
  }

  /**
    * A line containing no characters, or a line containing only spaces
    * (U+0020) or tabs (U+0009), is called a blank line.
    */
  def blankLine: Rule0 = rule {
    zeroOrMore(anyOf(" \t"))
  }

  def nonBlankLine: Rule0 = rule {
    zeroOrMore(anyOf(" \t")) ~ nonWhitespaceCharacter ~ zeroOrMore(noneOf("\n\r"))
  }

  /**
    * A whitespace character is a space (U+0020), tab (U+0009), newline
    * (U+000A), line tabulation (U+000B), form feed (U+000C), or carriage
    * return (U+000D).
    */
  val whitespaceCharacter = CharPredicate(" \t\n\u000b\f\r")

  /**
    * Whitespace is a sequence of one or more whitespace characters.
    */
  def whitespace = rule(oneOrMore(whitespaceCharacter))

  /**
    * A Unicode whitespace character is any code point in the Unicode Zs
    * class, or a tab (U+0009), carriage return (U+000D), newline (U+000A),
    * or form feed (U+000C).
    */
  val unicodeWhitespaceCharacter =
    CharPredicate("\t\r\n\f" + Unicode.Zs)

  /**
    * Unicode whitespace is a sequence of one or more Unicode whitespace
    * characters.
    */
  def unicodeWhitespace = rule(oneOrMore(unicodeWhitespaceCharacter))

  /**
    * A space is U+0020.
    */
  val space = CharPredicate(' ')

  /**
    * A non-whitespace character is any character that is not a whitespace
    * character.
    */
  val nonWhitespaceCharacter = unicodeWhitespaceCharacter.negated

  /**
    * An ASCII punctuation character is !, ", #, $, %, &, ', (, ), *, +, ,,
    * -, ., /, :, ;, <, =, >, ?, @, [, \, ], ^, _, `, {, |, }, or ~.
    */
  val asciiPunctuationCharacter =
    CharPredicate("!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")

  /**
    * A punctuation character is an ASCII punctuation character or anything
    * in the Unicode classes Pc, Pd, Pe, Pf, Pi, Po, or Ps.
    */
  val punctuationCharacter =
    asciiPunctuationCharacter ++
      Unicode.Pc ++ Unicode.Pd ++ Unicode.Pe ++ Unicode.Pf ++ Unicode.Pi ++
      Unicode.Po ++ Unicode.Ps

  def document: Rule1[Seq[Block]] = rule {
    zeroOrMore(blankLine) ~
      zeroOrMore(block).separatedBy(blankLine.+) ~
      blankLine.* ~ EOI
  }

  def block: Rule1[Block] = rule {
    (paragraph)
  }

  private def thematicBreak(char: Char): Rule0 = rule {
    optional(1 to 3 times space) ~ 3.times(char ~ space.*) ~ zeroOrMore(char ~ space.*)
  }

  /**
    * A line consisting of 0-3 spaces of indentation, followed by a sequence
    * of three or more matching -, _, or * characters, each followed
    * optionally by any number of spaces, forms a thematic break.
    */
  def thematicBreak: Rule1[ThematicBreak.type] = rule {
    (thematicBreak('-') | thematicBreak('_') | thematicBreak('*')) ~ push(ThematicBreak)
  }

  /**
    * An ATX heading consists of a string of characters, parsed as inline
    * content, between an opening sequence of 1–6 unescaped # characters and
    * an optional closing sequence of any number of unescaped # characters.
    * The opening sequence of # characters must be followed by a space or by
    * the end of line. The optional closing sequence of #s must be preceded
    * by a space and may be followed by spaces only. The opening # character
    * may be indented 0-3 spaces. The raw contents of the heading are
    * stripped of leading and trailing spaces before being parsed as inline
    * content. The heading level is equal to the number of # characters in
    * the opening sequence.
    */
  def atxHeading: Rule1[Heading] = rule {
    optional(1 to 3 times space) ~
      (( capture((1 to 6) times "#")) ~> (_.length)) ~ space ~
      (capture(noneOf("\n\r").*) ~ lineEnding ~>
        (_.trim.reverse.dropWhile(_ == '#').trim.reverse)) ~> Heading
  }

  /**
    * A setext heading consists of one or more lines of text, each containing
    * at least one non-whitespace character, with no more than 3 spaces
    * indentation, followed by a setext heading underline. The lines of text
    * must be such that, were they not followed by the setext heading
    * underline, they would be interpreted as a paragraph: they cannot be
    * interpretable as a code fence, ATX heading, block quote, thematic break,
    * list item, or HTML block.
    */
  def setextHeading: Rule1[Heading] = rule {
    capture(oneOrMore(nonBlankLine)) ~>
      (_.mkString("\n")) ~ setextHeadingUnderline ~>
        ((t: String,l: Int) => Heading(l,t))
  }

  /**
    * A setext heading underline is a sequence of = characters or a sequence
    * of - characters, with no more than 3 spaces indentation and any number
    * of trailing spaces. If a line containing a single - can be interpreted
    * as an empty list items, it should be interpreted this way and not as a
    * setext heading underline.
    */
  def setextHeadingUnderline: Rule1[Int] = rule {
    optional(1 to 3 times space) ~
      (oneOrMore('=') ~ push(1) | oneOrMore('-') ~ push(2)) ~ space.* ~
      lineEnding
  }

  /**
    * A sequence of non-blank lines that cannot be interpreted as other kinds
    * of blocks forms a paragraph. The contents of the paragraph are the
    * result of parsing the paragraph’s raw content as inlines. The
    * paragraph’s raw content is formed by concatenating the lines and
    * removing initial and final whitespace.
    */
  def paragraph: Rule1[Paragraph] = rule {
    oneOrMore(capture(nonBlankLine)).separatedBy(lineEnding) ~>
      ((x: Seq[String]) => Paragraph(x.map(_.trim).mkString("\n")))
  }
}
*/