package parser

// A shallow wrapper over another CharSequence (usually a String)
class SubSeq(s: CharSequence, start: Int, val length: Int) extends CharSequence:
  def this(s: CharSequence, start: Int) = this(s, start, s.length - start)

  def charAt(i: Int) = s.charAt(start + i)

  def subSequence(_start: Int, _end: Int) =
    if _start < 0 || _end < 0 || _end > length || _start > _end then
      throw new IndexOutOfBoundsException(s"start: ${_start}, end: ${_end}, length: $length")

    new SubSeq(s, start + _start, _end - _start)

  override def toString = s.subSequence(start, start + length).toString
