case class Matrix(grid: Seq[Seq[Int]]):
  def saddlePoints: Set[(Int, Int)] =
    val maxInRows = grid.map(_.maxByOption(identity).getOrElse(Int.MinValue)).toIndexedSeq
    val minInCols = grid.transpose.map(_.minByOption(identity).getOrElse(Int.MaxValue)).toIndexedSeq

    grid.zipWithIndex
      .flatMap((r, row) =>
        r.zipWithIndex.filter((x, col) => x == maxInRows(row) && x == minInCols(col)).map((_, col) => (row, col))
      )
      .toSet
