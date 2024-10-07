object NthPrime:
  def prime(n: Int): Option[Int] =
    if n == 0 then None
    else Some(nthPrime(n - 1, 2, Map.empty))

  /*
    We use a modified Sieve of Eratosthenes algorithm.

    We inspect one natural number at a time, and do one of the following:
    1. If we find a prime, we insert its square in a map, along with the
    prime as the value, so that we can generate the multiples if we need to.

    2. If we find a composite, we remove all the primes that are its factors,
    and insert the next multiples in the map.
   */
  private def nthPrime(n: Int, x: Int, composites: Map[Int, Seq[Int]]): Int =
    if composites.contains(x) then // Composite
      val c = composites(x)
        .foldLeft(composites.removed(x))((m, y) => m.insertWith(x + y, y))
      nthPrime(n, x + 1, c)
    else if n == 0 then x
    else // Prime
      val c = composites.insertWith(x * x, x)
      nthPrime(n - 1, x + 1, c)

  extension (m: Map[Int, Seq[Int]])
    private def insertWith(k: Int, v: Int): Map[Int, Seq[Int]] =
      m.updatedWith(k):
        case Some(xs) => Some(v +: xs)
        case _        => Some(Seq(v))
