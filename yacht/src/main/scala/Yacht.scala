object Yacht:
  def score(dices: List[Int], category: String): Int =
    val freq = dices.groupMapReduce(identity)(_ => 1)(_ + _)
    val oneToSix = Seq("ones", "twos", "threes", "fours", "fives", "sixes")
      .zip(1 to 6)
      .toMap

    category match
      case x if oneToSix.contains(x) =>
        val i = oneToSix(x)
        i * freq.get(i).getOrElse(0)
      case "full house" if freq.size == 2 =>
        freq
          .filter((_, x) => x == 2 || x == 3)
          .foldLeft(0)((acc, kv) => acc + kv._1 * kv._2)
      case "four of a kind" =>
        freq
          .filter((_, x) => x >= 4)
          .foldLeft(0)((_, kv) => kv._1 * 4)
      case "little straight" if freq.size == 5 && !freq.contains(6) => 30
      case "big straight" if freq.size == 5 && !freq.contains(1)    => 30
      case "choice"                                                 => dices.sum
      case "yacht" if freq.size == 1                                => 50
      case _                                                        => 0
