package bench

// Run with ./mill 'modules[prime-factors].jmh.runJmh'

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import scala.compiletime.uninitialized
import `<empty>`.{PrimeFactors, PrimeFactors2}

// See https://github.com/sbt/sbt-jmh/blob/main/plugin/src/sbt-test/sbt-jmh/run/src/main/scala/org/openjdk/jmh/samples/JMHSample_27_Params.scala
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(value = 1)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@State(Scope.Benchmark)
class PrimeFactorsBenchmark:
    @Param(Array("1", "2", "9", "8", "12", "901255", "93819012551"))
    var n: String = uninitialized

    @Benchmark
    def benchRecursion: Seq[Long] = PrimeFactors.factors(n.toLong)

    @Benchmark
    def benchLazyList: Seq[Long] = PrimeFactors2.factors(n.toLong)
