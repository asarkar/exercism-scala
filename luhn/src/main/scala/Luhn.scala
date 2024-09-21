object Luhn:
  def valid(s: String): Boolean =
    val xs = s
      .filter(c => c.isDigit || c.isSpaceChar)
      .reverse

    if xs.length() != s.length() then false
    else
      val ys = xs
        .filter(_.isDigit)
        .zipWithIndex
        .map(convert)

      ys.length > 1 && (ys.sum % 10) == 0

  private def convert(c: Char, i: Int): Int =
    val n   = c.asDigit
    val mul = i % 2 + 1
    n * mul - (if mul == 2 && n > 4 then 9 else 0)
