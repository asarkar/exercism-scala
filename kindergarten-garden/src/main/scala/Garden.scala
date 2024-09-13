enum Plant:
  case Clover, Grass, Radishes, Violets

private class Garden(m: Map[String, Array[Plant]]):
  def plants(student: String) = m(student)

import Plant.*

object Garden:
  private val students =
    Seq("Alice", "Bob", "Charlie", "David", "Eve", "Fred", "Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry")

  def defaultGarden(plants: String): Garden =
    val rows = plants
      .split("\\n")
      .map { r =>
        r.collect:
          case 'C' => Clover
          case 'G' => Grass
          case 'R' => Radishes
          case _   => Violets
        .grouped(2)
          .toArray
      }

    Garden(students.zip(rows.transpose.map(_.flatten)).toMap)
