object MatchingBrackets:
  private val pairs = Map(']' -> '[', '}' -> '{', ')' -> '(')

  def isPaired(s: String, stack: Seq[Char] = Seq.empty): Boolean =
    s match
      case "" => stack.isEmpty
      case _ =>
        val c = s.head
        if pairs.contains(c) && stack.headOption != pairs.get(c) then false
        else if pairs.values.exists(_ == c) then isPaired(s.tail, c +: stack)
        else if pairs.contains(c) then isPaired(s.tail, stack.tail)
        else isPaired(s.tail, stack)
