object CollatzConjecture:
  private def collatz(n: Int, count: Int): Int =
    if n == 1 then count
    else if n % 2 == 0 then collatz(n / 2, count + 1)
    else collatz(3 * n + 1, count + 1)

  def steps(n: Int): Option[Int] =
    if n <= 0 then None
    else Some(collatz(n, 0))
