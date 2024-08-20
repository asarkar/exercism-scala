object Etl:
  def transform(scoreMap: Map[Int, Seq[String]]): Map[String, Int] =
    scoreMap.toSeq.foldLeft(Map.empty[String, Int]) {(acc, kv) =>
      acc ++ kv._2.map(x => (x.toLowerCase(), kv._1))
    }
