// This is a slow and naive implementation,
// but an efficient implementation is far too complicated.
object PythagoreanTriplet:
  def isPythagorean(tp: (Int, Int, Int)): Boolean =
    val (a, b, c) = tp
    a < b && b < c && a * a + b * b == c * c

  def pythagoreanTriplets(start: Int, end: Int): Seq[(Int, Int, Int)] =
    (for {
      a <- start to (end.toDouble / math.sqrt(2)).toInt
      b <- a + 1 until end
      c <- b + 1 to end if isPythagorean((a, b, c))
    } yield ((a, b, c)))

  def pythagoreanTripletsSum(n: Int): Seq[(Int, Int, Int)] =
    for
      a <- 1 until n / 2
      b <- a + 1 until n
      c = n - a - b if isPythagorean((a, b, c))
    yield ((a, b, c))
