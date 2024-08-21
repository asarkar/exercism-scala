object Hamming:
  def distance(dnaStrandOne: String, dnaStrandTwo: String): Option[Int] =
    val m = dnaStrandOne.size
    val n = dnaStrandTwo.size

    if m == n then Some(dnaStrandOne.zip(dnaStrandTwo).count((c1, c2) => c1 != c2))
    else None
