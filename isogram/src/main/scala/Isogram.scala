object Isogram:
  def isIsogram(s: String): Boolean =
    val t = s.filter(_.isLetter).toLowerCase()
    t.toSet.size == t.size
