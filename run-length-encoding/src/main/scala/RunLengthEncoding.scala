import collection.mutable.StringBuilder

object RunLengthEncoding:
  def encode(s: String): String =
    val (x, y, j) = s.foldLeft((new StringBuilder(), '\u0000', 0)) { (acc, c) =>
      // It's stupid that nested tuples can't be deconstrcuted.
      val (t, prev, i) = acc

      if prev == c || i == 0 then (t, c, i + 1)
      else if i == 1 then (t.append(prev), c, 1)
      else (t.append(s"$i$prev"), c, 1)
    }
    if j == 1 then x.append(y)
    else if j > 1 then x.append(s"$j$y")

    x.toString()

  def decode(s: String): String =
    s.foldLeft((new StringBuilder(), 0)) { (acc, c) =>
      val (t, num) = acc

      if c.isDigit then (t, num * 10 + c.asDigit)
      else if num == 0 then (t.append(c), num)
      else
        for _ <- (1 to num) do t.append(c)
        (t, 0)
    }._1
      .toString()
