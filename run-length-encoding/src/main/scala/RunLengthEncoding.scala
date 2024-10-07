import collection.mutable.StringBuilder

object RunLengthEncoding:
  def encode(s: String): String =
    val (x, y, j) = s.foldLeft((new StringBuilder(), '\u0000', 0)) { case ((t, prev, cnt), c) =>
      if prev == c || cnt == 0 then (t, c, cnt + 1)
      else if cnt == 1 then (t.append(prev), c, 1)
      else (t.append(s"$cnt$prev"), c, 1)
    }
    if j == 1 then x.append(y)
    else if j > 1 then x.append(s"$j$y")

    x.toString()

  def decode(s: String): String =
    s.foldLeft((new StringBuilder(), 0)) { case ((t, num), c) =>
      if c.isDigit then (t, num * 10 + c.asDigit)
      else if num == 0 then (t.append(c), num)
      else
        t.append(c.toString() * num)
        (t, 0)
    }._1
      .toString()
