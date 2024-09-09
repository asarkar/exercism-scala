object AtbashCipher:
  private val mapping = ('a' to 'z').toIndexedSeq

  def encode(plain: String): String =
    val cipher = xcode(plain, mapping.size - 1 - _ + 'a')

    cipher.grouped(5).mkString(" ")

  def decode(cipher: String): String =
    xcode(cipher, 'z' - _)

  private def xcode(s: String, f: Char => Int): String =
    val text = for c <- s; x = f(c.toLower); if c.isLetterOrDigit
    yield (if c.isDigit then c else mapping(x))

    text.mkString
