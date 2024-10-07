type Frames = IndexedSeq[IndexedSeq[Int]]

class Bowling(rolls: Seq[Int] = Seq.empty):
  def roll(x: Int): Bowling = Bowling(x +: rolls)

  def score(): Either[String, Int] =
    val f                 = frames(rolls.reverse)
    val (over, numThrows) = isGameOver(f)
    val excessThrow       = numThrows < rolls.size
    val invalidScore      = f.exists(_.sum > 10)

    if (over && excessThrow) || invalidScore then Left("invalid roll")
    else if !over then Left("incomplete game")
    else Right(score(0, f))

  private def frames(rolls: Seq[Int]): Frames =
    rolls match
      case Seq()        => IndexedSeq.empty
      case Seq(x)       => IndexedSeq(IndexedSeq(x))
      case 10 +: xs     => IndexedSeq(10) +: frames(xs)
      case x +: y +: xs => IndexedSeq(x, y) +: frames(xs)

  private def isGameOver(frames: Frames): (Boolean, Int) =
    val n = frames.size

    val tenthFrame             = frames.lift(9)
    val tenthFrameNumThrows    = tenthFrame.map(_.size).getOrElse(0)
    val tenthFrameScore        = tenthFrame.map(_.sum).getOrElse(0)
    val completeInTenth        = tenthFrameNumThrows == 2 && tenthFrameScore < 10
    val eleventhFrameNumThrows = frames.lift(10).map(_.size).getOrElse(0)
    val completeInEleventh     = tenthFrameNumThrows + eleventhFrameNumThrows >= 3
    val numThrows              = frames.take(n).map(_.size).sum

    if completeInTenth then (true, 20)
    else if completeInEleventh then (true, 21)
    else if n >= 12 then (true, 21)
    else (false, numThrows)

  private def score(i: Int, frames: Frames): Int =
    frames match
      case Seq()        => 0
      case _ if i == 10 => 0
      case xs +: xxs =>
        val x = xs.sum
        val n = xs.size

        val bonus =
          if x == 10 && n == 1 then xxs.flatten.take(2).sum
          else if x == 10 then xxs.head.head
          else 0

        x + bonus + score(i + 1, xxs)
