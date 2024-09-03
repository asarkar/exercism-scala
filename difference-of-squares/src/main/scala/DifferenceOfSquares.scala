object DifferenceOfSquares:

  // https://helloacm.com/the-difference-between-sum-of-squares-and-square-of-the-sum/
  def sumOfSquares(n: Int): Int = (2 * n + 1) * (n + 1) * n / 6

  def squareOfSum(n: Int): Int =
    val s = n * (n + 1) / 2
    s * s

  def differenceOfSquares(n: Int): Int = squareOfSum(n) - sumOfSquares(n)
