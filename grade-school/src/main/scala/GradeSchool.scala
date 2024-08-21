import scala.collection.immutable.SortedMap

class School:
  type DB = Map[Int, Seq[String]]
  private var students = Set.empty[String]

  def add(name: String, grade: Int) =
    if !students.contains(name) then
      db = db.updatedWith(grade):
        case Some(names) => Some(names :+ name)
        case None        => Some(Seq(name))
      students = students + name

  var db: DB = SortedMap.empty[Int, Seq[String]]

  def grade(grade: Int): Seq[String] = db.getOrElse(grade, Seq.empty)

  def sorted: DB = db.toSeq.map((grade, names) => (grade, names.sorted)).toMap
