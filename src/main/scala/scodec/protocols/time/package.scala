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

package scodec
package protocols

import language.higherKinds

import fs2.Stream

package object time {

  /**
   * A single value in a `TimeSeries`. Provides a timestamp along with either a value of type `A` or
   * a clock tick (represented by a none).
   */
  type TimeSeriesValue[+A] = TimeStamped[Option[A]]

  /**
   * A stream of timestamped values or clock ticks.
   *
   * Values are represented as right values in a `TimeStamped[Option[A]]`, whereas
   * clock ticks are represented as nones. This encoding allows for an indication
   * of time passage with no observed values.
   *
   * Generally, time series appear in increasing order, and many combinators that work with
   * time series will rely on that. For streams that are globally ordered, but not locally ordered,
   * i.e., near adjacent values might be out of order but values at great distance from each other
   * are ordered, consider using `TimeStamped.reorderLocally` to adjust.
   */
  type TimeSeries[F[_], +A] = Stream[F, TimeSeriesValue[A]]

  /** Alias for a stream transducer on time series values. */
  type TimeSeriesTransducer[F[_], -A, +B] = Stream[F, TimeSeriesValue[A]] => Stream[F, TimeSeriesValue[B]]
}
