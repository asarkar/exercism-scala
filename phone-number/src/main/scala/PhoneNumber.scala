import scala.util.boundary, boundary.break, boundary.Label

// Direct Style
// https://medium.com/scala-3/scala-3-what-is-direct-style-d9c1bcb1f810
// https://contributors.scala-lang.org/t/usability-of-boundary-break-on-the-example-of-either-integration/
// https://www.youtube.com/watch?v=0Fm0y4K4YO8
private object optional:
  inline def apply[T](inline body: Label[None.type] ?=> T): Option[T] =
    boundary(Some(body))

  extension [T](r: Option[T])
    inline def ?(using label: Label[None.type]): T = r match
      case Some(x) => x
      case None    => break(None)

object PhoneNumber:
  def clean(s: String): Option[String] =
    val xs = s.filter(_.isDigit)
    val n  = xs.size

    if n == 11 && xs.head == '1' then check(xs.tail)
    else if n == 10 then check(xs)
    else None

  import optional.?
  private def check(s: String): Option[String] =
    optional:
      s.zipWithIndex.map(checkOne(_).?).mkString

  private val rules = (1 to 9).map(_ -> (0 to 9)).toMap ++ Map(0 -> (2 to 9), 3 -> (2 to 9))

  private def checkOne(x: (Char, Int)): Option[Char] =
    Option.when(rules(x._2).contains(x._1.asDigit))(x._1)
