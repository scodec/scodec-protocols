package fs2

import java.util.concurrent.ScheduledExecutorService

import scala.concurrent.duration._
import fs2.util.Task

object time {

  def awakeEvery(d: Duration)(
    implicit S: Strategy,
    scheduler: ScheduledExecutorService): Stream[Task, Duration] = ???

  def duration: Stream[Task, FiniteDuration] = ???

  def every(d: Duration): Stream[Task, Boolean] = ???

  def sleep(d: FiniteDuration)(
    implicit S: Strategy
    , schedulerPool: ScheduledExecutorService
    ): Stream[Task, Nothing] = ???
}
