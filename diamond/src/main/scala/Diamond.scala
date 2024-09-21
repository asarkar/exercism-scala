object Diamond:
  def rows(c: Char): Seq[String] =
    if c.isLetter && c.isUpper then
      val height = c - 'A' + 1
      val top    = (0 until height).map(row(height)(_))
      val bottom = top.reverse.tail

      top ++ bottom
    else Seq.empty

  private def row(n: Int)(i: Int): String =
    val c                = ('A' + i).toChar
    val k                = n - i - 1
    def go(x: Int): Char = if x == k then c else ' '
    val half             = Seq.unfold(0)(x => Option.when(x < n)((go(x), x + 1))).mkString
    half + (half.reverse.tail)
