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

import java.time.Instant
import scala.concurrent.duration._
import fs2._

class TimeSeriesTest extends ProtocolsSpec {

  def ts(value: Int) = TimeStamped(Instant.ofEpochSecond(value.toLong), value)

  test("interpolating time ticks in a timestamped stream") {
    val events = Stream(ts(1), ts(2), ts(3))
    val withTicksDefault = events.through(TimeSeries.interpolateTicks()).toList
    assertEquals(withTicksDefault, List(
      TimeStamped(Instant.ofEpochSecond(1), Some(1)),
      TimeStamped(Instant.ofEpochSecond(2), None),
      TimeStamped(Instant.ofEpochSecond(2), Some(2)),
      TimeStamped(Instant.ofEpochSecond(3), None),
      TimeStamped(Instant.ofEpochSecond(3), Some(3))
    ))
    val withTicks300ms = events.through(TimeSeries.interpolateTicks(300.millis)).toList
    assertEquals(withTicks300ms, List(
      TimeStamped(Instant.ofEpochSecond(1), Some(1)),
      TimeStamped(Instant.ofEpochMilli(1300), None),
      TimeStamped(Instant.ofEpochMilli(1600), None),
      TimeStamped(Instant.ofEpochMilli(1900), None),
      TimeStamped(Instant.ofEpochSecond(2), Some(2)),
      TimeStamped(Instant.ofEpochMilli(2200), None),
      TimeStamped(Instant.ofEpochMilli(2500), None),
      TimeStamped(Instant.ofEpochMilli(2800), None),
      TimeStamped(Instant.ofEpochSecond(3), Some(3))
    ))
  }
}
