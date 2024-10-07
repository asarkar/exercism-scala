object Series:
  def largestProduct(span: Int, input: String): Option[Int] =
    // Input validation and segmentation can be
    // done in a single pass but this is simpler.
    if span <= 0 || span > input.length() || input.exists(!_.isDigit) then None
    else
      input
        .split('0')
        .filter(_.length() >= span)
        .map(lp(span, _))
        .maxOption
        .orElse(Some(0))

  private def lp(n: Int, s: String): Int =
    s
      .sliding(n)
      .zipWithIndex
      .foldLeft((0, 0)) { case ((currPdt, maxPdt), (slice, i)) =>
        if i == 0 then
          val pdt = slice.map(_.asDigit).product
          (pdt, pdt)
        else
          val left  = s(i - 1)
          val right = s(i + n - 1)
          val pdt   = currPdt / left.asDigit * right.asDigit
          (pdt, math.max(pdt, maxPdt))
      }
      ._2
