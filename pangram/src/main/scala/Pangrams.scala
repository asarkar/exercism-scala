object Pangrams:
  extension (c: Char) def isAsciiLetter = c.isLetter && c <= 'z'

  def isPangram(input: String): Boolean =
    val xs = for c <- input if c.isAsciiLetter yield c.toLower
    xs.toSet.size == 26
