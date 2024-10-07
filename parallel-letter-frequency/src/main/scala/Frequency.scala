import scala.concurrent.{Await, Future, ExecutionContext, duration}
import duration.DurationInt
import java.util.concurrent.Executors

object Frequency:
  def frequency(numWorkers: Int, texts: Seq[String]): Map[Char, Int] =
    val executorService    = Executors.newFixedThreadPool(numWorkers)
    given ExecutionContext = ExecutionContext.fromExecutorService(executorService)

    val futures = texts.map(s => Future(freq(s)))
    val count   = Future.foldLeft(futures)(Map.empty[Char, Int])(combine)

    Await.result(count, 3.seconds)

  private def freq(s: String): Map[Char, Int] =
    s.filter(_.isLetter).groupMapReduce(_.toLower)(_ => 1)(_ + _)

  private def combine(m1: Map[Char, Int], m2: Map[Char, Int]): Map[Char, Int] =
    m1.foldLeft(m2) { case (m, (k, v)) => m + (k -> (v + m.getOrElse(k, 0))) }
