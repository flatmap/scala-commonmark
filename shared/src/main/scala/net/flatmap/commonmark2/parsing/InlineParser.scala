package net.flatmap.commonmark2.parsing

import net.flatmap.commonmark.HTMLEntities
import net.flatmap.commonmark2.Inline
import org.parboiled2._

case class InlineParser(input: ParserInput) extends Parser {
  import net.flatmap.commonmark2.CharPredicates._

  def escape: Rule1[String] = rule {
    '\\' ~ capture(ASCIIPunctuationCharacter)
  }

  def lineBreak: Rule1[Inline] = rule {
    ("\\\n" | "  \n") ~ push(Inline.LineBreak)
  }

  /// ENTITY ESCAPES ///////////////////////////////////////////////////////////

  def htmlEntity: Rule1[String] = rule {
    entityReference |
    decimalNumericCharacter |
    hexadecimalNumericCharacter
  }

  def entityReference: Rule1[String] = rule {
    capture('&' ~ CharPredicate.AlphaNum.* ~ ';') ~> { (name: String) =>
      val decoded = HTMLEntities.entities.get(name)
      test(decoded.isDefined) ~ push(decoded.get)
    }
  }

  def decimalNumericCharacter: Rule1[String] = rule {
    "&#" ~ capture((1 to 8).times(CharPredicate.Digit)) ~> { (s: String) =>
      val i = s.toInt
      if (i > 0 && java.lang.Character.isValidCodePoint(i))
        java.lang.Character.toChars(i).mkString
      else "\uFFFD"
    } ~ ';'
  }

  def hexadecimalNumericCharacter: Rule1[String] = rule {
    "&#" ~ ("X" | "x") ~
      capture((1 to 8).times(CharPredicate.HexDigit)) ~> { (s: String) =>
      val i = Integer.parseInt(s,16)
      if (i > 0 && java.lang.Character.isValidCodePoint(i))
        java.lang.Character.toChars(i).mkString
      else "\uFFFD"
    } ~ ';'
  }

  /// CODE SPANS ///////////////////////////////////////////////////////////////

  def backtickString: Rule1[Int] = rule {
    test(lastChar != '`') ~
    capture(oneOrMore('`'))  ~ !'`' ~> ((x: String) => x.length)
  }

  def matchingBacktickString(len: Int): Rule0 = rule {
    test(lastChar != '`') ~
    len.times('`') ~ !'`'
  }

  def nonMatchingBacktickString(len: Int): Rule0 = {
    val max = len - 1
    rule {
      test(lastChar != '`') ~
      (1 to max).times('`') ~ (!'`' | '`' ~ oneOrMore('`') ~ !'`')
    }
  }

  def codeSpan: Rule1[Inline] = rule {
    backtickString ~> { (l1: Int) =>
      capture {
        (noneOf("`") ~ optional(nonMatchingBacktickString(l1))).+
      } ~ matchingBacktickString(l1) ~> Inline.Code
    }
  }

  /// EMPHASIS /////////////////////////////////////////////////////////////////

  def delimiterRun: Rule1[String] = rule {
    capture {
      (oneOrMore('*') ~ !'*') |
      (oneOrMore('_') ~ !'_')
    }
  }

  def leftFlankingDelimiterRun: Rule1[String] = rule {
    (delimiterRun ~
      !(UnicodeWhitespaceCharacter | UnicodePunctuationCharacter)) |
    (test(
      cursor == 0 ||
      UnicodeWhitespaceCharacter(lastChar) ||
      UnicodePunctuationCharacter(lastChar))  ~
      delimiterRun ~ !UnicodeWhitespaceCharacter)
  }

  def rightFlankingDelimiterRun: Rule1[String] = rule {
    (test(
      cursor > 0 &&
      !UnicodeWhitespaceCharacter(lastChar) &&
      !UnicodePunctuationCharacter(lastChar)) ~
      delimiterRun
    ) |
    (test(
      cursor > 0 &&
      !UnicodeWhitespaceCharacter(lastChar)) ~
      delimiterRun ~
      &(UnicodeWhitespaceCharacter | UnicodePunctuationCharacter))
  }
}