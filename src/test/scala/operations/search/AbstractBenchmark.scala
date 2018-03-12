package operations.search

import org.scalameter.api._
import org.scalameter.picklers.Implicits._

/**
  * Created by Dorian Thiessen on 2018-03-12.
  */
abstract class AbstractBenchmark extends Bench[Double] {
  /* Configuration */
  lazy val executor = SeparateJvmsExecutor(
    new Executor.Warmer.Default,
    Aggregator.min[Double],
    measurer)
  lazy val measurer = new Measurer.Default
  lazy val reporter = new LoggingReporter[Double]
  lazy val persistor: Persistor.None.type = Persistor.None
}
