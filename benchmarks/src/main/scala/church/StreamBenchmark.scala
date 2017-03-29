package church

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit


class StreamBenchmark {
  @Benchmark
    @BenchmarkMode(Array(Mode.AverageTime))
    @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def zipAndAdd(): Int = {
    val input1 = Iterator.range(0, 10000000)
    val input2 = Iterator.range(0, 10000000)

    val ones1 = Stream.fromIterator(input1).map(_ => 1)
    val ones2 = Stream.fromIterator(input2).map(_ => 1)
    val sum = ones1.zip(ones2).map{ case (a, b) => a + b }
    sum.foldLeft(0){ _ + _ }
  }
}
