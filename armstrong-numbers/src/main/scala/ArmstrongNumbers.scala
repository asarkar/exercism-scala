import scala.math.pow

object ArmstrongNumbers:
  def isArmstrongNumber(n: Int): Boolean =
    val digits = IndexedSeq.unfold(n)(x => Option.when(x > 0)((x % 10, x / 10)))
    digits.map(x => pow(x, digits.length).intValue).sum == n
