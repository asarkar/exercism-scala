object OcrNumbers:
  def convert(lines: Seq[String]): String =
    // Process 4 rows at a time.
    lines
      .grouped(4)
      .map(parseN)
      .mkString(",")

  val nums =
    Seq(
      Seq(
        " _ ",
        "| |",
        "|_|",
        "   "
      ),
      Seq(
        "   ",
        "  |",
        "  |",
        "   "
      ),
      Seq(
        " _ ",
        " _|",
        "|_ ",
        "   "
      ),
      Seq(
        " _ ",
        " _|",
        " _|",
        "   "
      ),
      Seq(
        "   ",
        "|_|",
        "  |",
        "   "
      ),
      Seq(
        " _ ",
        "|_ ",
        " _|",
        "   "
      ),
      Seq(
        " _ ",
        "|_ ",
        "|_|",
        "   "
      ),
      Seq(
        " _ ",
        "  |",
        "  |",
        "   "
      ),
      Seq(
        " _ ",
        "|_|",
        "|_|",
        "   "
      ),
      Seq(
        " _ ",
        "|_|",
        " _|",
        "   "
      )
    )

  private def parseN(xs: Seq[String]): String =
    if xs.exists(_.size % 3 != 0) then "?"
    else
      // Process 3 columns at a time.
      val (num, rest) = xs.map(_.splitAt(3)).unzip
      val n           = nums.zipWithIndex.find(_._1 == num)
      val x           = n.map(_._2.toString()).getOrElse("?")

      if rest.exists(!_.isBlank()) then x ++ parseN(rest)
      else x
