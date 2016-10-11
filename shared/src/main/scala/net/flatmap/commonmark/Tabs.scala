package net.flatmap.commonmark

object Tabs {
  val meaningfulTabs1 = "^( {0,3}(>|\\*|\\+|-|#+))?[\t ]*\t[\t ]*".r

  def replaceMeaningfulTabs(s: String) =
    meaningfulTabs1.findFirstMatchIn(s).fold {
      s
    } { m =>
      m.matched.zipWithIndex.flatMap {
        case ('\t',i) => List.fill(4 - i % 4)(' ')
        case (c,_) => List(c)
      }.mkString + s.drop(m.matched.length)
    }
}
