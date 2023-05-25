package dotty.tools.benchmarks.lazyvals

import org.openjdk.jmh.annotations.*
import LazyVals.LazyHolder
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 5)
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class UninitializedAccess:

  @Benchmark
  def measureInitialized(bh: Blackhole) =
    var i = 0
    val holder = new LazyHolder
    bh.consume(holder)
    bh.consume(holder.value)
    i = i + 1
