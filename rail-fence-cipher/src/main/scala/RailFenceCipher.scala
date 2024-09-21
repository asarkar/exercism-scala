object RailFenceCipher:
  def encode(text: String, numRails: Int): String =
    val rails = text
      .filterNot(_.isSpaceChar)
      .zip(indices(numRails))
      .groupMap(_._2)(_._1)

    (0 until numRails)
      .map(n => rails(n).mkString)
      .mkString

  def decode(text: String, numRails: Int): String =
    Seq
      .unfold(rails(text, numRails)) { xxs =>
        xxs match
          case Seq()         => None
          case last +: Seq() => Some((last, Seq()))
          case init :+ last =>
            val (ys, zzs) = init.map(s => (s.head, s.tail)).unzip
            Some((ys.mkString, last +: zzs.filterNot(_.isEmpty).reverse))
      }
      .mkString

  private def indices(n: Int): Iterator[Int] =
    val xs = (0 until n).toList
    Iterator.unfold(xs)(ys => Some((ys.init, ys.reverse))).flatten

  private def rails(s: String, n: Int): Seq[String] =
    val railLengths = indices(n)
      .take(s.length())
      .toSeq
      .groupMapReduce(identity)(_ => 1)(_ + _)

    Seq.unfold((0, s)) { (i, t) =>
      Option.when(i < n):
        val (left, right) = t.splitAt(railLengths(i))
        (left, (i + 1, right))
    }
