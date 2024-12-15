package reader

trait Reader[+T]:
  /** If this is a reader over character sequences, the underlying char sequence. If not, throws a `NoSuchMethodError`
    * exception.
    *
    * @throws [[java.lang.NoSuchMethodError]]
    *   if this not a char sequence reader.
    */
  def source: java.lang.CharSequence =
    throw new NoSuchMethodError("not a char sequence reader")

  def offset: Int =
    throw new NoSuchMethodError("not a char sequence reader")

  /** Returns the first element of the reader
    */
  def first: T

  /** Returns an abstract reader consisting of all elements except the first
    *
    * @return
    *   If `atEnd` is `true`, the result will be `this`; otherwise, it's a `Reader` containing more elements.
    */
  def rest: Reader[T]

  /** Returns an abstract reader consisting of all elements except the first `n` elements.
    */
  def drop(n: Int): Reader[T] =
    (1 to n).foldLeft(this) { case (r, _) => r.rest }

  /** `true` iff there are no more elements in this reader.
    */
  def atEnd: Boolean
