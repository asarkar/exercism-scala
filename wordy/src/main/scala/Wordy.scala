object Wordy:
  def answer(question: String): Option[Int] =
    val (n, _, s) = parseNum(question)
    eval(n, s)

  private def eval(x: Option[Int], text: String): Option[Int] =
    if x.isEmpty then x
    else
      val (y, op, s) = parseNum(text)
      if y.isEmpty && text == "?" then x
      else
        val z = x.zip(y)
        val n = op match
          case "plus"                       => z.map(_ + _)
          case "minus"                      => z.map(_ - _)
          case "multiplied by"              => z.map(_ * _)
          case "divided by" if y != Some(0) => z.map(_ / _)
          case "raised to the"              => z.map(math.pow(_, _).intValue)
          case _                            => None

        eval(n, s)

  private def parseNum(text: String): (Option[Int], String, String) =
    val num = raw"[-]?\d+".r

    num.findFirstMatchIn(text) match
      case None => (None, "", "")
      case Some(m) =>
        val n      = m.matched.trim().toIntOption
        val prefix = text.take(m.start).trim()
        val suffix = text.drop(m.end).trim()
        (n, prefix, suffix)
