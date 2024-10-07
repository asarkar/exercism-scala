// Number of primes less than or equal to x
// https://t5k.org/howmany.html#table
object PrimeFactors:
  /*
  ---
  Benchmark using JMH
  ---
          (n)  Mode  Cnt       Score      Error  Units
            1  avgt    5       2.627 ±    0.014  ns/op
            2  avgt    5       5.437 ±    0.076  ns/op
            9  avgt    5      14.499 ±    0.041  ns/op
            8  avgt    5      32.360 ±    0.854  ns/op
           12  avgt    5      35.915 ±    1.420  ns/op
       901255  avgt    5     423.328 ±    3.809  ns/op
  93819012551  avgt    5  576300.891 ± 4219.941  ns/op
   */
  def factors(i: Long, divisor: Long = 2L, primes: Seq[Long] = Seq.empty): Seq[Long] =
    // Note that we can't stop when divisor > √n, because although one prime factor
    // < √n, the other factor > √n may also be prime and needs to be tested.
    if (i == 1)
      primes
    else if (i % divisor == 0)
      factors(i / divisor, 2L, primes :+ divisor)
    else
      factors(i, divisor + 1L, primes)

import scala.util.boundary, boundary.break

object PrimeFactors2:
  private def isPrime(n: Long): Boolean =
    if n < 2 then false
    else
      val x = math.sqrt(n.toDouble).toLong
      var k = 2L
      boundary:
        while k <= x do
          if n % k == 0 then break(false)
          k += 1
        true

  private val primes = LazyList.unfold(1L) { n =>
    var k = n
    while !isPrime(k) do k += 1
    Some((k, k + 1))
  }

  /*
  ---
  Benchmark using JMH
  ---
          (n)  Mode  Cnt       Score      Error  Units
            1  avgt    5       5.532 ±    0.076  ns/op
            2  avgt    5      12.145 ±    0.105  ns/op
            9  avgt    5      27.411 ±    0.585  ns/op
            8  avgt    5      23.905 ±    0.508  ns/op
           12  avgt    5      27.543 ±    0.151  ns/op
       901255  avgt    5     380.775 ±    9.364  ns/op
  93819012551  avgt    5  310279.593 ± 1896.503  ns/op
   */
  def factors(n: Long, p: LazyList[Long] = primes): Seq[Long] =
    p match
      case Seq() => Seq()
      // Only the head is evaluated; the tail is
      // evaluated in the next recursive call only
      // if the 'else' branch is executed.
      case xs @ (k +: ys) =>
        if n > 1 && n % k == 0 then k +: factors(n / k, xs)
        else if n <= 1 then Seq.empty
        else factors(n, ys)
