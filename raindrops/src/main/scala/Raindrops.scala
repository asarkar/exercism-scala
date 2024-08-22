object Raindrops:
  def convert(n: Int): String =
    val xs = Seq((3, "Pling"), (5, "Plang"), (7, "Plong"))
    val ys = for (x, y) <- xs if n % x == 0 yield y

    if ys.isEmpty then n.toString()
    else ys.mkString
