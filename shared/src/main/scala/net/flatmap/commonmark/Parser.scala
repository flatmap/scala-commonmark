package net.flatmap.commonmark

import java.util.regex.Pattern

import org.parboiled2._
import shapeless._

/**
  * Created by martin on 07/10/2016.
  */
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
  def line[T](r: Rule1[T]): Rule1[T] =
    rule(r ~ (lineEnding | EOI))

  /**
    * A line ending is a newline (U+000A), a carriage return (U+000D) not
    * followed by a newline, or a carriage return and a following newline.
    */
  def lineEnding: Rule0 = rule('\n' | '\r' ~ '\n'.?)

  /**
    * A line containing no characters, or a line containing only spaces
    * (U+0020) or tabs (U+0009), is called a blank line.
    */
  def blankLine: Rule0 = rule(anyOf(" \t") ~ (lineEnding | EOI))

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

  def nonBlankLine: Rule0 = rule {
    anyOf(" \t\u000b\f").* ~ nonWhitespaceCharacter.+ ~ noneOf("\n\r").*
  }

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
    blankLine.* ~!~ block.*.separatedBy(lineEnding ~ blankLine.*) ~ blankLine
      .* ~ EOI
  }

  def block: Rule1[Block] = rule {
    (thematicBreak) |
    (atxHeading) |
    (paragraph)
  }

  /**
    * A line consisting of 0-3 spaces of indentation, followed by a sequence
    * of three or more matching -, _, or * characters, each followed
    * optionally by any number of spaces, forms a thematic break.
    */
  def thematicBreak: Rule1[ThematicBreak.type] = rule {
    optional(1 to 3 times space) ~
      (('-' ~ space.* ~ '-' ~ space.* ~ '-' ~ zeroOrMore('-').separatedBy(space.*)) |
       ('_' ~ space.* ~ '_' ~ space.* ~ '_' ~ zeroOrMore('_').separatedBy(space.*)) |
       ('*' ~ space.* ~ '*' ~ space.* ~ '*' ~ zeroOrMore('*').separatedBy(space.*))) ~
      space.* ~ push(ThematicBreak)
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
      (capture(noneOf("\n\r").*) ~>
        (_.trim.reverse.dropWhile(_ == '#').trim.reverse)) ~> Heading
  }





  def paragraph: Rule1[Paragraph] = rule {
    capture(nonBlankLine).+.separatedBy(lineEnding) ~>
      ((x: Seq[String]) => Paragraph(x.map(_.trim).mkString("\n")))
  }
}
