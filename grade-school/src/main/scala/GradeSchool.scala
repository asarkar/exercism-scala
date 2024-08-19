import scala.math.Ordering.Implicits.seqOrdering

class School:
  type DB = Map[Int, Seq[String]]

  def add(name: String, grade: Int) =
    db = db.updatedWith(grade): 
      case Some(names) => Some(names :+ name)
      case None => Some(Seq(name))

  var db: DB = Map.empty[Int, Seq[String]]

  def grade(grade: Int): Seq[String] = db.getOrElse(grade, Seq.empty)

  def sorted: DB = db.toSeq.sorted.map(kv => (kv._1, kv._2.sorted)).toMap

