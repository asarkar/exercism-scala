import collection.mutable.Map

class FaceValueMap(dominoes: Array[(Int, Int)]):
  private val map = Map.empty[Int, Set[Int]]
  dominoes.indices.foreach(release)

  def release(i: Int): Unit =
    dominoes(i).toList.foreach { x =>
      map.updateWith(x):
        case None        => Some(Set(i))
        case Some(value) => Some(value.+(i))
    }

  def acquire(i: Int): Unit =
    dominoes(i).toList.foreach { x =>
      map.updateWith(x):
        case Some(value) => Some(value.-(i))
        case _           => None
    }

  def candidates(i: Int, left: Boolean): Set[Int] =
    val (l, r) = dominoes(i)
    val side   = if left then l else r
    map(side).filterNot(_ == i)
