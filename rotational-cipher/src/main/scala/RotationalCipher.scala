object RotationalCipher:
  def rotate(s: String, n: Int): String =
    s.map(shift(_, math.floorMod(n, 26)))

  private def shift(c: Char, n: Int): Char =
    if c.isLetter then
      val a = if c.isUpper then 'A' else 'a'
      (a + (c.toInt + n - a) % 26).toChar
    else c
