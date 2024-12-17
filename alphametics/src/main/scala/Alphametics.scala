class Alphametics private (equation: Array[String], result: String, nonZeroLetters: Set[Char]):
  private def solve(row: Int, col: Int, carry: Int, solution: Map[Char, Int]): Option[Map[Char, Int]] =
    val addend = row < equation.length
    val word   = if addend then equation(row) else result
    val n      = word.length

    if addend && col >= n then solve(row + 1, col, carry, solution)
    else if !addend && col == n then Option.when(carry == 0)(solution)
    else
      val letter   = word(col)
      val assigned = solution.contains(letter)

      if addend then
        if assigned then solve(row + 1, col, carry + solution(letter), solution)
        else
          val used = solution.values.toSet
          Iterator
            .range(0, 10)
            .filterNot(i => used.contains(i) || (nonZeroLetters.contains(letter) && i == 0))
            .flatMap(i => solve(row + 1, col, carry + i, solution + (letter -> i)))
            .nextOption()
      else
        val sumDigit = carry % 10

        if assigned && solution.get(letter).contains(sumDigit) then solve(0, col + 1, carry / 10, solution)
        else if !assigned then
          val used = solution.values.exists(_ == sumDigit)
          if used || (sumDigit == 0 && nonZeroLetters.contains(letter)) then None
          else solve(0, col + 1, carry / 10, solution + (letter -> sumDigit))
        else None

object Alphametics:
  def solve(puzzle: String): Option[Map[Char, Int]] =
    Alphametics.parse(puzzle).flatMap { case (equation, result) =>
      if equation.exists(_.length > result.length) then None
      else
        val nonZeroLetters = equation.map(_.last).toSet + result.last
        new Alphametics(equation, result, nonZeroLetters).solve(0, 0, 0, Map.empty)
    }

  private def parse(puzzle: String): Option[(Array[String], String)] =
    puzzle.filterNot(_.isWhitespace).split("==") match
      case Array(xs, result, _*) =>
        val equation = xs.split("\\+").map(_.reverse)
        Some(((equation, result.reverse)))
      case _ => None
