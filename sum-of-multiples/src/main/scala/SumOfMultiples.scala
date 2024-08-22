object SumOfMultiples:
  def sum(factors: Set[Int], limit: Int): Int =
    val xs = for x <- factors yield fact(x, limit)
    val ys = xs.flatten.groupBy(identity)

    ys.keys.sum

  private def fact(n: Int, limit: Int): Seq[Int] =
    if n == 0 then Seq(0)
    else
      Seq.unfold(1) { x =>
        val y = x * n
        Option.when(y < limit)((y, x + 1))
      }
