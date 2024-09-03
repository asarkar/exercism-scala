object BeerSong:
  def recite(start: Int, n: Int): String =
    (start until (start - n) by -1).map(verse).mkString("\n")

  private def numBottles(n: Int): String =
    n match
      case 0 => "No more bottles"
      case 1 => "1 bottle"
      case _ => s"${n} bottles"

  private def numBottlesLeft(n: Int): String =
    n match
      case 0 => "99 bottles"
      case 1 => "no more bottles"
      case _ => numBottles(n - 1)

  private def beginningOf2ndLine(n: Int): String =
    n match
      case 0 => "Go to the store and buy some more"
      case 1 => "Take it down and pass it around"
      case _ => "Take one down and pass it around"

  def verse(n: Int): String =
    val x          = numBottles(n)
    val firstLine  = s"${x} of beer on the wall, ${x.toLowerCase()} of beer.\n"
    val secondLine = s"${beginningOf2ndLine(n)}, ${numBottlesLeft(n)} of beer on the wall.\n"

    firstLine + secondLine
