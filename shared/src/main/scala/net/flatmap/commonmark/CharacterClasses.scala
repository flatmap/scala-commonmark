package net.flatmap.commonmark

/**
  * @see http://spec.commonmark.org/0.26/#characters-and-lines
  */
object CharacterClasses {
  type CharacterClass = Char => Boolean

  /**
    * A character is a Unicode code point. Although some code points (for
    * example, combining accents) do not correspond to characters in an
    * intuitive sense, all code points count as characters for purposes of
    * this spec.
    */
  val Character: CharacterClass = _ => true


  /**
    * A whitespace character is a space (U+0020), tab (U+0009), newline
    * (U+000A), line tabulation (U+000B), form feed (U+000C), or carriage
    * return (U+000D).
    */
  val WhitespaceCharacter: CharacterClass = { ch =>
    ch == ' ' || ch == '\t' || ch == '\n' || ch == '\u000b' || ch == '\u000c' || ch == '\r'
  }

  /**
    * A Unicode whitespace character is any code point in the Unicode Zs class,
    * or a tab (U+0009), carriage return (U+000D), newline (U+000A), or form
    * feed (U+000C).
    */
  val UnicodeWhitespaceCharacter: CharacterClass = { ch =>
    ch == '\t' || ch == '\r' || ch == '\n' || ch == '\u000c' ||
      Unicode.Zs.contains(ch)
  }

  /**
    * A space is U+0020.
    */
  val Space: CharacterClass = _ == ' '


  /**
    * A non-whitespace character is any character that is not a whitespace
    * character.
    */
  val NonWhitespaceCharacter: CharacterClass =
    WhitespaceCharacter andThen (!_)

  /**
    * An ASCII punctuation character is !, ", #, $, %, &, ', (, ), *, +, ,, -,
    * ., /, :, ;, <, =, >, ?, @, [, \, ], ^, _, `, {, |, }, or ~.
    */
  val ASCIIPunctuationCharacter: CharacterClass = { ch =>
    ch == '!' || ch == '"' || ch == '#' || ch == '$' ||
    ch == '%' || ch == '&' || ch == '\'' || ch == '(' ||
    ch == ')' || ch == '*' || ch == '+' || ch == ',' ||
    ch == '-' || ch == '*' || ch == '.' || ch == '/' ||
    ch == ':' || ch == ';' || ch == '<' || ch == '=' ||
    ch == '>' || ch == '?' || ch == '@' || ch == '[' ||
    ch == '\\' || ch == ']' || ch == '^' || ch == '_' ||
    ch == '`' || ch == '{' || ch == '|' || ch == '}' ||
    ch == '~'
  }

  /**
    * A punctuation character is an ASCII punctuation character or anything in
    * the Unicode classes Pc, Pd, Pe, Pf, Pi, Po, or Ps.
    */
  val UnicodePunctuationCharacter: CharacterClass = { ch =>
    ASCIIPunctuationCharacter(ch) ||
      Unicode.Pc.contains(ch) ||
      Unicode.Pd.contains(ch) ||
      Unicode.Pe.contains(ch) ||
      Unicode.Pf.contains(ch) ||
      Unicode.Pi.contains(ch) ||
      Unicode.Po.contains(ch) ||
      Unicode.Ps.contains(ch)
  }

  val IndentationCharacter: CharacterClass = { ch =>
    ch == ' ' || ch == '\t'
  }
}
