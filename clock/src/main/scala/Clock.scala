object Clock:
  // https://docs.scala-lang.org/scala3/book/types-opaque-types.html
  opaque type Clock = (Int, Int)

  // Integer division truncated toward negative infinity
  private def floorDiv(x: Int, y: Int) =
    val q = x / y
    if (q * y == x || math.signum(x) == math.signum(y)) then q
    else q - 1

  def apply(hour: Int, minutes: Int): Clock =
    val hr  = Math.floorMod(hour, 24)
    val min = Math.floorMod(minutes, 60)
    val h   = floorDiv(minutes, 60)
    val x   = Math.floorMod(hr + h, 24)

    (x, min)

  def apply(minutes: Int): Clock =
    apply(0, minutes)

  extension (x: Clock)
    def +(y: Clock): Clock = Clock(x._1 + y._1, x._2 + y._2)
    def -(y: Clock): Clock = Clock(x._1 - y._1, x._2 - y._2)
