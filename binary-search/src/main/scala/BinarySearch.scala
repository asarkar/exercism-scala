object BinarySearch:
  private def search(lo: Int, hi: Int, xs: IndexedSeq[Int], x: Int): Option[Int] =
    val mid = lo + (hi - lo + 1) / 2

    if hi < lo then None
    else if xs(mid) == x then Some(mid)
    else if xs(mid) > x then search(lo, mid - 1, xs, x)
    else search(mid + 1, hi, xs, x)

  def find(xs: Seq[Int], x: Int): Option[Int] =
    val ys = xs.toIndexedSeq
    search(0, ys.size - 1, ys, x)
