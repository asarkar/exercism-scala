object Darts:
  def score(x: Double, y: Double): Int =
    val dist = math.sqrt(x * x + y * y).ceil.toInt

    if dist <= 1 then 10
    else if dist <= 5 then 5
    else if dist <= 10 then 1
    else 0
