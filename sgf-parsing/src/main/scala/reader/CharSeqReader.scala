package reader

/** An object encapsulating basic character constants.
  */
object CharSeqReader:
  final val EofCh = '\u001a'

/** A character array reader reads a stream of characters (keeping track of their positions) from an array.
  *
  * @param source
  *   the source sequence
  * @param offset
  *   starting offset.
  */
class CharSeqReader(override val source: java.lang.CharSequence, override val offset: Int) extends Reader[Char]:

  /** Construct a `CharSeqReader` with its first element at `source(0)` and position `(1,1)`.
    */
  def this(source: java.lang.CharSequence) = this(source, 0)

  /** Returns the first element of the reader, or EofCh if reader is at its end.
    */
  def first: Char =
    if offset < source.length then source.charAt(offset) else CharSeqReader.EofCh

  /** Returns a CharSeqReader consisting of all elements except the first.
    *
    * @return
    *   If `atEnd` is `true`, the result will be `this`; otherwise, it's a `CharSeqReader` containing the rest of input.
    */
  def rest: CharSeqReader =
    if offset < source.length then new CharSeqReader(source, offset + 1)
    else this

  /** true iff there are no more elements in this reader (except for trailing EofCh's)
    */
  def atEnd: Boolean = offset >= source.length

  /** Returns an abstract reader consisting of all elements except the first `n` elements.
    */
  override def drop(n: Int) = new CharSeqReader(source, offset + n)

  /** Returns a String in the form `CharSeqReader(first, ...)`, or `CharSeqReader()` if this is `atEnd`.
    */
  override def toString: String =
    val c = if atEnd then "" else s"'$first', ..."
    s"CharSeqReader($c)"
