import scala.util.boundary, boundary.break

object RnaTranscription:
  // private val rules = Map('G' -> 'C', 'C' -> 'G', 'T' -> 'A', 'A' -> 'U')

  // Shorter but not fail-fast
  // def toRna(s: String): Option[String] =
  //   val (l, r) = s.partitionMap(rules.get(_).toRight('x'))
  //   Option.when(l.isEmpty())(r)

  // Direct Style
  // https://www.scala-lang.org/api/3.3.0/scala/util/boundary$.html
  // https://medium.com/scala-3/scala-3-what-is-direct-style-d9c1bcb1f810
  // https://www.youtube.com/watch?v=0Fm0y4K4YO8
  def toRna(s: String): Option[String] =
    boundary:
      val xs = s.collect:
        case 'G' => 'C'
        case 'C' => 'G'
        case 'T' => 'A'
        case 'A' => 'U'
        case _   => break(None)
      Some(xs)
