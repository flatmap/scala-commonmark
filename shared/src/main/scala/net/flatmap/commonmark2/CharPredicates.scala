package net.flatmap.commonmark2

import org.parboiled2._

/**
  * @see http://spec.commonmark.org/0.26/#characters-and-lines
  */
object CharPredicates {
  /**
    * A character is a Unicode code point. Although some code points (for
    * example, combining accents) do not correspond to characters in an
    * intuitive sense, all code points count as characters for purposes of
    * this spec.
    */
  val Character = CharPredicate.from(_ => true)


  /**
    * A whitespace character is a space (U+0020), tab (U+0009), newline
    * (U+000A), line tabulation (U+000B), form feed (U+000C), or carriage
    * return (U+000D).
    */
  val WhitespaceCharacter = CharPredicate(" \t\n\u000b\u000c\r")

  /**
    * A Unicode whitespace character is any code point in the Unicode Zs class,
    * or a tab (U+0009), carriage return (U+000D), newline (U+000A), or form
    * feed (U+000C).
    */
  val UnicodeWhitespaceCharacter = CharPredicate("\t\r\n\u000c") ++ Unicode.Zs

  /**
    * A space is U+0020.
    */
  val Space = CharPredicate(' ')

  /**
    * A non-whitespace character is any character that is not a whitespace
    * character.
    */
  val NonWhitespaceCharacter = WhitespaceCharacter.negated

  /**
    * An ASCII punctuation character is !, ", #, $, %, &, ', (, ), *, +, ,, -,
    * ., /, :, ;, <, =, >, ?, @, [, \, ], ^, _, `, {, |, }, or ~.
    */
  val ASCIIPunctuationCharacter = CharPredicate(
    "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
  )

  /**
    * A punctuation character is an ASCII punctuation character or anything in
    * the Unicode classes Pc, Pd, Pe, Pf, Pi, Po, or Ps.
    */
  val UnicodePunctuationCharacter =
    Unicode.Pc ++ Unicode.Pd ++ Unicode.Pe ++ Unicode.Pf ++
      Unicode.Pi ++ Unicode.Po ++ Unicode.Ps

  val IndentationCharacter = CharPredicate(" \t")
}
