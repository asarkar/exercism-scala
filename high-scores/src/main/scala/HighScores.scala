object HighScores:
  def latest(scores: Seq[Int]): Int       = scores.last
  def personalBest(scores: Seq[Int]): Int = scores.max
  def personalTop(scores: Seq[Int]): Seq[Int] =
    scores.sorted(summon[Ordering[Int]].reverse).take(3)

  def report(scores: Seq[Int]): String =
    val last = latest(scores)
    val best = personalBest(scores)

    val s =
      if last < best then s" ${best - last} short of "
      else " "

    s"Your latest score was $last. That's${s}your personal best!"
