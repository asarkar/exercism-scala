object Grains:
  def square(n: Int): Option[BigInt] =
    Option.when((n > 0) && (n <= 64))(BigInt(2).pow(n - 1))

  def total = BigInt("18446744073709551615")
