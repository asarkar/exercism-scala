import collection.mutable.Map

object Change:
  private type Coins  = Seq[Int]
  private type Change = Option[IndexedSeq[Int]]

  val memo = Map.empty[Int, Change]

  def findFewestCoins(amount: Int, denominations: Coins): Change =
    memo.clear()

    change(amount, denominations.sorted)
      .map(_.sorted)

  private def change(amount: Int, denominations: Coins): Change =
    // Can make zero change with no coins.
    if amount == 0 then Some(IndexedSeq.empty)
    // Cannot make change without any coins.
    else if denominations.isEmpty then None
    else if memo.contains(amount) then memo(amount)
    else
      val candidates = denominations.takeWhile(_ <= amount)
      // Try each coin.
      val coins = candidates.foldLeft(Seq.empty[Change]) { (acc, x) =>
        change(amount - x, denominations)
          .map(x +: _) +: acc
      }
      // Use IndexedSeq because size on Seq takes O(n) time.
      val fewestCoins = coins.minByOption(_.map(_.size).getOrElse(Int.MaxValue)).flatten
      memo(amount) = fewestCoins
      fewestCoins
