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

  def htmlEntity: Rule1[String] = rule {
    entityReference |
    decimalNumericCharacter |
    hexadecimalNumericCharacter
  }

  def entityReference: Rule1[String] = rule {
    capture('&' ~ CharPredicate.AlphaNum.* ~ ';') ~> { name =>
      val decoded = HTMLEntities.entities.get(name)
      test(decoded.isDefined) ~ push(decoded.get)
    }
  }

  def decimalNumericCharacter: Rule1[String] = rule {
    "&#" ~ capture((1 to 8).times(CharPredicate.Digit)) ~> { s =>
      val i = s.toInt
      if (i > 0 && java.lang.Character.isValidCodePoint(i))
        java.lang.Character.toChars(i).mkString
      else "\uFFFD"
    } ~ ';'
  }

  def hexadecimalNumericCharacter: Rule1[String] = rule {
    "&#" ~ ("X" | "x") ~
      capture((1 to 8).times(CharPredicate.HexDigit)) ~> { s =>
      val i = Integer.parseInt(s,16)
      if (i > 0 && java.lang.Character.isValidCodePoint(i))
        java.lang.Character.toChars(i).mkString
      else "\uFFFD"
    } ~ ';'
  }

  def backtickString: Rule1[Int] = rule {
    capture(oneOrMore('`'))  ~ !'`' ~> (_.length)
  }
}