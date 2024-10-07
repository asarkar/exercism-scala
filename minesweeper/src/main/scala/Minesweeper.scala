object Minesweeper:
  def annotate(board: Seq[String]): Seq[String] =
    board.zipWithIndex
      .map((row, r) =>
        row.zipWithIndex.map { (ch, c) =>
          if ch == ' ' then countMines(board, r, c)
          else ch
        }.mkString
      )

  private def neighbors(r: Int, c: Int): Seq[(Int, Int)] =
    for {
      x <- -1 to 1;
      y <- -1 to 1; if (x, y) != (0, 0)
    } yield (r + x, c + y)

  private def countMines(board: Seq[String], r: Int, c: Int): Char =
    val i = neighbors(r, c)
      .map((x, y) => board.lift(x).flatMap(_.lift(y)).getOrElse('.'))
      .count(_ == '*')
    if i > 0 then (i + '0').toChar else ' '
