import scala.util.boundary
import scala.util.boundary.{Label, break}
import scala.annotation.{implicitNotFound, nowarn}

// Direct Style
// https://medium.com/scala-3/scala-3-what-is-direct-style-d9c1bcb1f810
// https://contributors.scala-lang.org/t/usability-of-boundary-break-on-the-example-of-either-integration/
// https://www.youtube.com/watch?v=0Fm0y4K4YO8
private object either:
  case class Fail[+A](a: A)

  // Suppress [E165] Type Warning - pattern selector should be an instance of Matchable
  // https://docs.scala-lang.org/scala3/reference/other-new-features/matchable.html
  @nowarn inline def apply[A, T](inline body: Label[Fail[A]] ?=> T): Either[A, T] =
    boundary(body) match
      case Fail(a: A) => Left(a)
      case t: T       => Right(t)

  extension [A, B, T](t: Either[A, B])(using
      @implicitNotFound("Get a label!") b: Label[either.Fail[A]]
  )
    inline def value: B =
      t match
        case Left(a)  => break(either.Fail(a))
        case Right(b) => b

class DNA(s: String):
  private def count(c: Char): Either[String, (Char, Int)] =
    if "ACGT".contains(c) then Right(c, 1) else Left("Error")
  private val zeroCounts = IndexedSeq(('A', 0), ('C', 0), ('G', 0), ('T', 0))

  import either.value
  def nucleotideCounts: Either[String, Map[Char, Int]] =
    val x = either:
      // Tests expect the keys to be always present
      s.map(count(_).value) ++ zeroCounts

    x match
      case Right(xs) => Right(xs.groupMapReduce(_._1)(_._2)(_ + _))
      case Left(e)   => Left(e)
