class Accumulate:
  def accumulate[A, B](f: (A) => B, list: List[A]): List[B] =
    list match
      case head :: next => f(head) :: accumulate(f, next)
      case Nil          => Nil
