import scala.compiletime.asMatchable

object FlattenArray:
  def flatten(xs: Seq[Any]): Seq[Any] =
    xs
      .filterNot(_ == null)
      .flatMap { x =>
        x.asMatchable match
          case ys: Seq[Any] => flatten(ys)
          case elem         => Seq(elem)
      }
