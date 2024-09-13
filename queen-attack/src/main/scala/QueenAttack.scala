import compiletime.asMatchable

class Queen(val row: Int, val col: Int):
  override def equals(that: Any): Boolean =
    that.asMatchable match
      case q: Queen =>
        row == q.row && col == q.col
      case _ => false

  override def hashCode(): Int = row.hashCode + 31 * col

object Queen:
  def create(row: Int, col: Int): Option[Queen] =
    if row >= 0 && row < 8 && col >= 0 && col < 8 then Some(Queen(row, col))
    else None

object QueenAttack:
  def canAttack(q1: Queen, q2: Queen): Boolean =
    val r = math.abs(q1.row - q2.row)
    val c = math.abs(q1.col - q2.col)
    r == 0 || c == 0 || r == c
