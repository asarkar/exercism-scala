object AllYourBase:
  def rebase(inputBase: Int, nums: Seq[Int], outputBase: Int): Option[Seq[Int]] =
    if inputBase < 2 || outputBase < 2 then None
    else
      decimal(inputBase, nums.reverse)
        .map(rebase(outputBase, _))

  private def rebase(base: Int, decimal: Int): Seq[Int] =
    if decimal == 0 then Seq(0)
    else Seq.unfold(decimal)(d => Option.when(d > 0)((d % base, d / base))).reverse

  private def decimal(base: Int, nums: Seq[Int]): Option[Int] =
    nums match
      case Seq()                        => Some(0)
      case x +: _ if x < 0 || x >= base => None
      case x +: xs                      => decimal(base, xs).map(x + base * _)
