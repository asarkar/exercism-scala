package parser

import scala.util.matching.Regex
import reader.CharSeqReader

/** The ''most important'' differences between `RegexParsers` and [[parser.Parsers]] are:
  *
  *   - `Elem` is defined to be [[scala.Char]]
  *   - There's an implicit conversion from [[java.lang.String]] to `Parser[String]`, so that string literals can be
  *     used as parser combinators.
  *   - There's an implicit conversion from [[scala.util.matching.Regex]] to `Parser[String]`, so that regex expressions
  *     can be used as parser combinators.
  *   - The parsing methods call the method `skipWhitespace` (defaults to `true`) and, if true, skip any whitespace
  *     before each parser is called.
  *   - Protected val `whiteSpace` returns a regex that identifies whitespace.
  */
trait RegexParsers extends Parsers:
  type Elem = Char

  protected val whiteSpace = """\s+""".r

  def skipWhitespace = whiteSpace.toString.length > 0

  /** Method called to handle whitespace before parsers.
    *
    * It checks `skipWhitespace` and, if true, skips anything matching `whiteSpace` starting from the current offset.
    *
    * @param source
    *   The input being parsed.
    * @param offset
    *   The offset into `source` from which to match.
    * @return
    *   The offset to be used for the next parser.
    */
  protected def handleWhiteSpace(source: java.lang.CharSequence, offset: Int): Int =
    val skip =
      if skipWhitespace then
        whiteSpace.findPrefixMatchOf(new SubSeq(source, offset)) match
          case Some(matched) => matched.end
          case None          => 0
      else 0
    offset + skip

  import ParseResult.*

  /** A parser that matches a literal string */
  given Conversion[String, Parser[String]] = s =>
    new Parser[String]:
      def apply(in: Input) =
        val source = in.source
        val offset = in.offset
        val start  = handleWhiteSpace(source, offset)
        val i = Iterator
          .from(0)
          .dropWhile(i => i < s.size && (start + i) < source.length && s.charAt(i) == source.charAt((start + i)))
          .next()
        val j = start + i
        if i == s.size then Success(source.subSequence(start, j).toString, in.drop(j - offset))
        else
          val rest = in.drop(start - offset)
          if start == source.length() then Failure(s"'$s' expected but end of source found", rest)
          else Failure(s"'$s' expected but '${source.charAt(j)}' found at index $j", rest)

  /** A parser that matches a regex string */
  given Conversion[Regex, Parser[String]] = r =>
    new Parser[String]:
      def apply(in: Input) =
        val source = in.source
        val offset = in.offset
        val start  = handleWhiteSpace(source, offset)
        r.findPrefixMatchOf(new SubSeq(source, start)) match
          case Some(matched) =>
            Success(source.subSequence(start, start + matched.end).toString, in.drop(start + matched.end - offset))
          case None =>
            val found = if start == source.length() then "end of source" else s"'${source.charAt(start)}'"
            Failure(s"string matching regex '$r' expected but $found found", in.drop(start - offset))

  import scala.language.implicitConversions

  /** A parser generator delimiting whole phrases (i.e. programs).
    *
    * `phrase(p)` succeeds if `p` succeeds and no input is left over after `p`.
    *
    * @param p
    *   the parser that must consume all input for the resulting parser to succeed.
    *
    * @return
    *   a parser that has the same result as `p`, but that only succeeds if `p` consumed all the input.
    */
  override def phrase[T](p: Parser[T]): Parser[T] =
    super.phrase(p <~ "")

  /** Parse some prefix of reader `in` with parser `p`. */
  def parse[T](p: Parser[T], in: java.lang.CharSequence): ParseResult[T] =
    p(new CharSeqReader(in))

  /** Parse all of character sequence `in` with parser `p`. */
  def parseAll[T](p: Parser[T], in: java.lang.CharSequence): ParseResult[T] =
    parse(phrase(p), in)
