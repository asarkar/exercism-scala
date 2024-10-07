object SpiralMatrix:
  def spiralMatrix(n: Int): Seq[Seq[Int]] =
    spiral(n).zipWithIndex.sorted
      .map(_._2 + 1)
      // grouped can't handle n=0
      .grouped(math.max(n, 1))
      .toSeq

  // Generates the coordinates of the outer layer in spiral order,
  // then recurses into the inner matrix.
  private def spiral(n: Int, row: Int = 0, col: Int = 0): Seq[(Int, Int)] =
    if n <= 0 then Seq.empty
    else if n == 1 then Seq((row, col))
    else
      val topRow    = for c <- col until (col + n) yield (row, c)
      val rightCol  = for r <- (row + 1) until (row + n) yield (r, col + n - 1)
      val bottomRow = for c <- (col + n - 2) to col by -1 yield (row + n - 1, c)
      val leftCol   = for r <- (row + n - 2) until row by -1 yield (r, col)

      topRow ++ rightCol ++ bottomRow ++ leftCol ++ spiral(n - 2, row + 1, col + 1)
