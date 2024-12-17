package parser
import reader.Reader

/** `Parsers` is a component that ''provides'' generic parser combinators.
  *
  * There are two abstract members that must be defined in order to produce parsers: the type `Elem` and
  * [[parser.Parsers.Parser]]. There are helper methods that produce concrete `Parser` implementations.
  *
  * A `Parsers` may define multiple `Parser` instances, which are combined to produced the desired parser.
  *
  * The type of the elements these parsers should parse must be defined by declaring `Elem` (each parser is polymorphic
  * in the type of result it produces).
  *
  * There are two aspects to the result of a parser:
  *   1. success or failure
  *   1. the result.
  *
  * A [[parser.Parsers.Parser]] produces both kinds of information, by returning a [[parser.Parsers.ParseResult]] when
  * its `apply` method is called on an input.
  *
  * The term ''parser combinator'' refers to the fact that these parsers are constructed from primitive parsers and
  * composition operators, such as sequencing, alternation, optionality, repetition, lifting, and so on. For example,
  * given `p1` and `p2` of type [[parser.Parsers.Parser]]:
  *
  * {{{
  *  p1 ~ p2    // sequencing: must match p1 followed by p2
  *  p1 | p2    // alternation: must match either p1 or p2, with preference given to p1
  *  opt(p1)    // optionality: may match p1 or not
  *  rep(p1)    // repetition: matches any number of repetitions of p1
  * }}}
  *
  * These combinators are provided as methods on [[parser.Parsers.Parser]], or as methods taking one or more `Parsers`
  * and returning a `Parser` provided in this class.
  *
  * A ''primitive parser'' is a parser that accepts or rejects a single piece of input, based on a certain criterion,
  * such as whether the input...
  *   - satisfies a certain predicate (see method `acceptIf`),
  *   - or other conditions, by using one of the other methods available, or subclassing `Parser`
  *
  * Even more primitive parsers always produce the same result, irrespective of the input. See method `success` as
  * example.
  *
  * @see
  *   [[parser.RegexParsers]] and other known subclasses for practical examples.
  */
// https://github.com/scala/scala-parser-combinators
trait Parsers:
  /** the type of input elements the provided parsers consume (When consuming invidual characters, a parser is typically
    * called a ''scanner'', which produces ''tokens'' that are consumed by what is normally called a ''parser''.
    * Nonetheless, the same principles apply, regardless of the input type.)
    */
  type Elem

  /** The parser input is an abstract reader of input elements, i.e. the type of input the parsers in this component
    * expect.
    */
  type Input = Reader[Elem]

  /** A base class for parser results. A result is either successful or not. On success, provides a result of type `T`
    * which consists of some result (and the rest of the input).
    */
  enum ParseResult[+T]:
    case Success[T](result: T, in: Input) extends ParseResult[T]
    case Failure(msg: String, in: Input)  extends ParseResult[Nothing]

  /** A wrapper over sequence of matches.
    *
    * Given `p1: Parser[A]` and `p2: Parser[B]`, a parser composed with `p1 ~ p2` will have type `Parser[~[A, B]]`. The
    * successful result of the parser can be extracted from this case class.
    *
    * It also enables pattern matching, so something like this is possible:
    *
    * {{{
    *  def concat(p1: Parser[String], p2: Parser[String]): Parser[String] =
    *    p1 ~ p2 ^^ { case a ~ b => a + b }
    * }}}
    */
  // https://stackoverflow.com/a/6819299/839733
  // https://users.scala-lang.org/t/tilde-class-in-parser-combinators/4908/5
  case class ~[+A, +B](_1: A, _2: B):
    override def toString = s"(${_1}~${_2})"

  import ParseResult.*

  /** The root class of parsers. Parsers are functions from the Input type to ParseResult.
    */
  abstract class Parser[+T] extends (Input => ParseResult[T]) { p =>
    def apply(in: Input): ParseResult[T]

    /** A parser combinator for sequential composition.
      *
      * `p ~ q` succeeds if `p` succeeds and `q` succeeds on the input left over by `p`.
      *
      * @param q
      *   a parser that will be executed after `p` (this parser) succeeds -- evaluated at most once, and only when
      *   necessary.
      * @return
      *   a `Parser` that -- on success -- returns a `~` (like a `Pair`, but easier to pattern match on) that contains
      *   the result of `p` and that of `q`. The resulting parser fails if either `p` or `q` fails.
      */
    def ~[U](q: => Parser[U]): Parser[T ~ U] = in =>
      p(in) match
        case Success(x, in1) =>
          q(in1) match
            case Success(y, rest)       => Success(new ~(x, y), rest)
            case f @ Failure(msg, rest) => f
        case f @ Failure(msg, in1) => f

    /** A parser combinator for function application.
      *
      * `p ^^ f` succeeds if `p` succeeds; it returns `f` applied to the result of `p`.
      *
      * @param f
      *   a function that will be applied to this parser's result (see `map` in `ParseResult`).
      * @return
      *   a parser that has the same behaviour as the current parser, but whose result is transformed by `f`.
      */
    def ^^[U](f: T => U): Parser[U] = in =>
      p(in) match
        case Success(result, rest)  => Success(f(result), rest)
        case f @ Failure(msg, rest) => f

    /** A parser combinator for sequential composition which keeps only the left result.
      *
      * `p <~ q` succeeds if `p` succeeds and `q` succeeds on the input left over by `p`.
      *
      * @note
      *   <~ has lower operator precedence than ~ or ~>.
      *
      * @param q
      *   a parser that will be executed after `p` (this parser) succeeds -- evaluated at most once, and only when
      *   necessary
      * @return
      *   a `Parser` that -- on success -- returns the result of `p`.
      */
    def <~[U](q: => Parser[U]): Parser[T] =
      (p ~ q) ^^ { case x ~ _ => x }

    /** A parser combinator for sequential composition which keeps only the right result.
      *
      * `p ~> q` succeeds if `p` succeeds and `q` succeeds on the input left over by `p`.
      *
      * @param q
      *   a parser that will be executed after `p` (this parser) succeeds -- evaluated at most once, and only when
      *   necessary.
      * @return
      *   a `Parser` that -- on success -- returns the result of `q`.
      */
    def ~>[U](q: => Parser[U]): Parser[U] =
      (p ~ q) ^^ { case _ ~ x => x }

    /** A parser combinator for alternative composition.
      *
      * `p | q` succeeds if `p` succeeds or `q` succeeds. Note that `q` is only tried if `p`s failure is non-fatal
      * (i.e., back-tracking is allowed).
      *
      * @param q
      *   a parser that will be executed if `p` (this parser) fails (and allows back-tracking)
      * @return
      *   a `Parser` that returns the result of the first parser to succeed (out of `p` and `q`) The resulting parser
      *   succeeds if (and only if)
      *   - `p` succeeds, ''or''
      *   - if `p` fails allowing back-tracking and `q` succeeds.
      */
    def |[U >: T](q: => Parser[U]): Parser[U] = in =>
      p(in) match
        case s @ Success(_, _) => s
        case _                 => q(in)

    /** A parser combinator that parameterizes a subsequent parser with the result of this one.
      *
      * Use this combinator when a parser depends on the result of a previous parser. `p` should be a function that
      * takes the result from the first parser and returns the second parser.
      *
      * `p into fq` (with `fq` typically `{x => q}`) first applies `p`, and then, if `p` successfully returned result
      * `r`, applies `fq(r)` to the rest of the input.
      *
      * ''From: G. Hutton. Higher-order functions for parsing. J. Funct. Program., 2(3):323--343, 1992.''
      *
      * @example
      *   {{{def perlRE = "m" ~> (".".r into (separator => """[^%s]*""".format(separator).r <~ separator))}}}
      *
      * @param fq
      *   a function that, given the result from this parser, returns the second parser to be applied
      * @return
      *   a parser that succeeds if this parser succeeds (with result `x`) and if then `fq(x)` succeeds
      */
    def >>[U](fq: T => Parser[U]): Parser[U] = in =>
      p(in) match
        case Success(result, rest)  => fq(result)(rest)
        case f @ Failure(msg, rest) => f
  }

  /** A helper method that turns a `Parser` into one that will print debugging information to stdout before and after
    * being applied.
    */
  def log[T](p: => Parser[T])(name: String): Parser[T] = in =>
    val r = p(in)
    println(s"$name --> $r")
    r

  /** A parser matching input elements that satisfy a given predicate.
    *
    * `elem(kind, p)` succeeds if the input starts with an element `e` for which `p(e)` is true.
    *
    * @param kind
    *   The element kind, used for error messages
    * @param p
    *   A predicate that determines which elements match.
    * @return
    */
  def elem(kind: String, p: Elem => Boolean): Parser[Elem] = acceptIf(p)(_ => s"$kind expected")

  /** A parser matching input elements that satisfy a given predicate.
    *
    * `acceptIf(p)(el => "Unexpected "+el)` succeeds if the input starts with an element `e` for which `p(e)` is true.
    *
    * @param err
    *   A function from the received element into an error message.
    * @param p
    *   A predicate that determines which elements match.
    * @return
    *   A parser for elements satisfying p(e).
    */
  def acceptIf(p: Elem => Boolean)(err: Elem => String): Parser[Elem] = in =>
    if in.atEnd then Failure("end of input", in)
    else if p(in.first) then Success(in.first, in.rest)
    else Failure(err(in.first), in)

  /** A parser that always succeeds.
    *
    * @param v
    *   The result for the parser
    * @return
    *   A parser that always succeeds, with the given result `v`
    */
  def success[T](v: T): Parser[T] = in => Success(v, in)

  /** A parser generator for optional sub-phrases.
    *
    * `opt(p)` is a parser that returns `Some(x)` if `p` returns `x` and `None` if `p` fails.
    *
    * @param p
    *   A `Parser` that is tried on the input
    * @return
    *   a `Parser` that always succeeds: either with the result provided by `p` or with the empty result
    */
  def opt[T](p: => Parser[T]): Parser[Option[T]] = p ^^ (Some(_)) | success(None)

  /** A parser generator for repetitions.
    *
    * `rep(p)` repeatedly uses `p` to parse the input until `p` fails (the result is a List of the consecutive results
    * of `p`).
    *
    * @param p
    *   a `Parser` that is to be applied successively to the input
    * @return
    *   A parser that returns a list of results produced by repeatedly applying `p` to the input.
    */
  def rep[T](p: => Parser[T]): Parser[Seq[T]] = in =>
    lazy val p0 = p // lazy argument

    def continue(in1: Input): ParseResult[Seq[T]] =
      p0(in1) match
        case Success(x, in2) =>
          continue(in2) match
            case Success(y, rest) => Success(x +: y, rest)
            case f                => f
        case _ => Success(Seq.empty, in1)

    continue(in)

  /** A parser generator for non-empty repetitions.
    *
    * `rep1(p)` repeatedly uses `p` to parse the input until `p` fails -- `p` must succeed at least once (the result is
    * a `List` of the consecutive results of `p`)
    *
    * @param p
    *   a `Parser` that is to be applied successively to the input
    * @return
    *   A parser that returns a list of results produced by repeatedly applying `p` to the input (and that only succeeds
    *   if `p` matches at least once).
    */
  def rep1[T](p: => Parser[T]): Parser[Seq[T]] =
    p ~ rep(p) ^^ { case x ~ xs => x +: xs }

  /** A parser generator delimiting whole phrases (i.e. programs).
    *
    * `phrase(p)` succeeds if `p` succeeds and no input is left over after `p`.
    *
    * @param p
    *   the parser that must consume all input for the resulting parser to succeed.
    * @return
    *   a parser that has the same result as `p`, but that only succeeds if `p` consumed all the input.
    */
  def phrase[T](p: Parser[T]): Parser[T] = in =>
    p(in) match
      case s @ Success(_, in1) => if in1.atEnd then s else Failure("end of input expected", in1)
      case f                   => f
