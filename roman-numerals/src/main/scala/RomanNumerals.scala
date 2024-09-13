object RomanNumerals:
  private val mapping = Seq(
    ("M", 1000),
    ("CM", 900),
    ("D", 500),
    ("CD", 400),
    ("C", 100),
    ("XC", 90),
    ("L", 50),
    ("XL", 40),
    ("X", 10),
    ("IX", 9),
    ("V", 5),
    ("IV", 4),
    ("I", 1)
  )
  def roman(n: Int): String =
    Iterator.unfold(n)(i => mapping.find(_._2 <= i).map((s, k) => (s, i - k))).mkString
