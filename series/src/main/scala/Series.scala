object Series:
  def slices(n: Int, s: String): Seq[Seq[Int]] =
    s.sliding(n).map(_.map(_.asDigit)).toSeq
