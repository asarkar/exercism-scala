enum NumberType:
  case Perfect, Abundant, Deficient

object PerfectNumbers:
  /*
  It can be shown that the sum of the divisors of a number
  can be expressed as the product of the sum of the powers
  of the prime factors of the number.

  Example:
  n = 18, sum of divisors = 1 + 2 + 3 + 6 + 9 + 18
  = 2^0 x 3^0 + 2^1 x 3^0 + 2^0 x 3^1 +
    2^1 x 361 + 2^0 x 3^2 + 2^1 x 3^2
  = (2^0 + 2^1) x (3^0 + 3^1 + 3^2)
  = (1 + p1) x (1 + p2 + p2^2), where p1=2, and p2=3

  So, the task reduces to finding all the prime factors
  and the product of the sum of their powers.

  Furthermore, the highest power of a factor is the
  number of times it divides the "remaining" number.
  "Remaining" means the result after progressively
  dividing the original number with the smaller prime
  factors.

  In the example above, the highest power of 2 is 1.
  The highest power of 3 is 2.
   */
  def classify(n: Int): Either[String, NumberType] =
    if n <= 0 then Left("Classification is only possible for natural numbers.")
    else if n <= 3 then Right(NumberType.Deficient)
    else
      val rt   = math.sqrt(n).intValue
      val xs   = (2 to rt).scanLeft((1, n))(sumOfPowers).tail
      val x    = xs.last._2
      val pdt1 = xs.map(_._1).product
      val pdt2 = pdt1 * (if x > 2 then 1 + x else 1)
      val pdt3 = pdt2 - n

      val y =
        if pdt3 < n then NumberType.Deficient
        else if pdt3 > n then NumberType.Abundant
        else NumberType.Perfect

      Right(y)

  private def sumOfPowers(x: (Int, Int), i: Int): (Int, Int) =
    val (k, n) = x
    val q      = n / i
    val rem    = n % i
    if rem == 0 then
      val (x, y) = sumOfPowers((k + 1, q), i)
      (math.pow(i, k).intValue + x, y)
    else (1, n)
