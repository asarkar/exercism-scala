import math.Integral.Implicits.infixIntegralOps
import scala.collection.Searching.*

object Say:
  private val MAPPING = IndexedSeq(
    (0L, "zero"),
    (1L, "one"),
    (2L, "two"),
    (3L, "three"),
    (4L, "four"),
    (5L, "five"),
    (6L, "six"),
    (7L, "seven"),
    (8L, "eight"),
    (9L, "nine"),
    (10L, "ten"),
    (11L, "eleven"),
    (12L, "twelve"),
    (13L, "thirteen"),
    (14L, "fourteen"),
    (15L, "fifteen"),
    (16L, "sixteen"),
    (17L, "seventeen"),
    (18L, "eighteen"),
    (19L, "nineteen"),
    (20L, "twenty"),
    (30L, "thirty"),
    (40L, "forty"),
    (50L, "fifty"),
    (60L, "sixty"),
    (70L, "seventy"),
    (80L, "eighty"),
    (90L, "ninety"),
    (100L, "hundred"),
    (1_000L, "thousand"),
    (1_000_000L, "million"),
    (1_000_000_000L, "billion"),
    (1_000_000_000_000L, "trillion")
  )

  def inEnglish(n: Long): Option[String] =
    if n < 0L || n > 999_999_999_999L then None
    else if n == 0 then Some("zero")
    else
      val (x, word) = MAPPING.search((n, ""))(Ordering.by(_._1)) match
        case Found(i)          => MAPPING(i)
        case InsertionPoint(i) => MAPPING(i - 1)

      val (a, b)      = n /% x
      val (left, sep) = if n < 100 then (Some(word), "-") else (inEnglish(a).map(l => s"$l $word"), " ")
      if b == 0
      then left
      else left.zip(inEnglish(b)).map((l, r) => s"$l$sep$r")
