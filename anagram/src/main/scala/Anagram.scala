object Anagram:
  def findAnagrams(target: String, candidates: Seq[String]): Seq[String] =
    val s = target.toLowerCase()
    val t = s.sorted

    candidates
      .groupBy(_.toLowerCase().sorted)
      .getOrElse(t, Seq.empty)
      .filter(_.toLowerCase() != s)
