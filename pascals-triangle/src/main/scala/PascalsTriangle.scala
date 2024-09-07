/*
Identity: C(n,k+1) = C(n,k) * (n-k) / (k+1), where n starts from 0.
We start with C(n,0) = 1, and calculate the rest using the identity.
But wait...each row is mirrored around the middle element, so,
we only need to calculate up to the middle element. Then we
flip the row and append to itself.
 */
object PascalsTriangle:
  def rows(n: Int): Seq[Seq[Int]] =
    if n <= 0 then Seq.empty else (0 to n - 1).map(row)

  private def row(n: Int): Seq[Int] =
    val mid = n / 2
    val left = 1 :: Iterator
      .unfold((0, 1)) { (i, x) =>
        val a = n - i
        val b = i + 1
        val y = x * a / b
        Some((y, (b, y)))
      }
      .take(mid)
      .toList

    val x     = mid + (n % 2)
    val right = left.take(x).reverse

    left ++ right
