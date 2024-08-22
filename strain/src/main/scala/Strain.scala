object Strain:
  def keep[A](xs: Seq[A], p: A => Boolean): Seq[A] =
    for x <- xs if p(x)
    yield x

  def discard[A](xs: Seq[A], p: A => Boolean): Seq[A] =
    keep(xs, !p(_))
