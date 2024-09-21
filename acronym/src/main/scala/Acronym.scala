object Acronym:
  private def isFirst(curr: Char, prev: Char) = curr.isLetter && (prev == ' ' || prev == '-')
  private def isUpper(curr: Char, prev: Char) = curr.isUpper && !prev.isUpper

  /*
  1. If a character is the start of a word, take it. Words are separated by whitespaces.
  2. If a character is preceded by a hyphen, take it.
  3. If a character is upper case and not preceded by another upper case character, take it.
   */
  def abbreviate(phrase: String): String =
    (for (c, i) <- phrase.zipWithIndex if i == 0 || isFirst(c, phrase(i - 1)) || isUpper(c, phrase(i - 1))
    yield c.toUpper).mkString
