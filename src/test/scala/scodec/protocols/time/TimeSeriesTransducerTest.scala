/*
 * Copyright (c) 2013, Scodec
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package scodec.protocols
package time

import fs2._
import java.time.Instant

class TimeSeriesTransducerTest extends ProtocolsSpec {

  test("support combining two transducers via an either") {
    val add1: Transform.Aux[Unit, Int, Int] = Transform.lift(_ + 1)
    val add2: Transform.Aux[Unit, Int, Int] = Transform.lift(_ + 2)
    val x: Transform.Aux[Unit, Either[Int, Int], Int] = add1.choice(add2).xmapState(_._1)(u => (u,u))
    val source: TimeSeries[Pure, Either[Int, Int]] =
      Stream(
        TimeStamped(Instant.ofEpochMilli(0), Right(1)),
        TimeStamped(Instant.ofEpochMilli(500), Left(2)),
        TimeStamped(Instant.ofEpochMilli(1500), Right(3))
      ).through(TimeSeries.interpolateTicks())
    assertEquals(source.through(TimeSeries.preserve(x).toPipe).toList, List(
      TimeSeriesValue(Instant.ofEpochMilli(0), 3),
      TimeSeriesValue(Instant.ofEpochMilli(500), 3),
      TimeSeriesValue.tick(Instant.ofEpochMilli(1000)),
      TimeSeriesValue(Instant.ofEpochMilli(1500), 5)))
  }
}
