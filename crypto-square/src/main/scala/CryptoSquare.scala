object CryptoSquare:
  def ciphertext(s: String): String =
    val t = s.filter(_.isLetterOrDigit).map(_.toLower)
    if t.isEmpty() then t
    else
      val col    = math.sqrt(t.size).ceil.intValue
      val rect   = t.grouped(col)
      val cipher = rect.map(_.padTo(col, ' ')).toSeq.transpose

      cipher.map(_.mkString).mkString(" ")
