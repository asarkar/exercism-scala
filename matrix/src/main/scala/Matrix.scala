import scala.collection.immutable.ArraySeq
case class Matrix(s: String):
  val rows = s.split('\n').map(_.split("\\s+").map(_.toInt))
  val cols = rows.transpose

  def row(n: Int): IndexedSeq[Int] =
    if n < 0 || n >= rows.size then IndexedSeq.empty
    else ArraySeq.unsafeWrapArray(rows(n))

  def column(n: Int): IndexedSeq[Int] =
    if n < 0 || n >= cols.size then IndexedSeq.empty
    else ArraySeq.unsafeWrapArray(cols(n))
