type Palindrome = Option[(Int, Set[(Int, Int)])]

case class PalindromeProducts(min: Int, max: Int):
  def smallest: Palindrome = outer(_ <= _, min to max)

  def largest: Palindrome = outer(_ >= _, max to min by -1)

  private def outer(op: (Int, Int) => Boolean, rng: Range, pal: Palindrome = None): Palindrome =
    if rng.isEmpty then pal
    else
      rng
        .scanLeft(Right(pal).withLeft[Palindrome])((p, r) => palindrome(op, rng.head, r, p.getOrElse(None)))
        .takeWhile(_.isRight) match
        case IndexedSeq() => pal
        case xs           => outer(op, rng.tail, xs.last.getOrElse(None))

  private def isPal(n: Int): Boolean =
    val s = n.toString()
    s == s.reverse

  private def orderedTuple(x: Int, y: Int): (Int, Int) =
    if x < y then (x, y) else (y, x)

  private def palindrome(
      op: (Int, Int) => Boolean,
      left: Int,
      right: Int,
      pal: Palindrome
  ): Either[Palindrome, Palindrome] =
    val pdt = left * right
    if pal.filter((p, _) => !op(pdt, p)).isDefined then Left(None)
    else if isPal(pdt) then
      val xs = pal.filter(_._1 == pdt).map(_._2).getOrElse(Set.empty)
      Right(Some((pdt, xs + orderedTuple(left, right))))
    else Right(pal)
