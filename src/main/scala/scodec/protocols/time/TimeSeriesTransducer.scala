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

import language.higherKinds

/** Companion for [[TimeSeriesTransducer]]. */
object TimeSeriesTransducer {

  def lift[F[_], A, B](f: A => B): TimeSeriesTransducer[F, A, B] =
    _.map(_.map(_.map(f)))

  def drainRight[F[_], L, R]: TimeSeriesTransducer[F, Either[L, R], L] = _.collect {
    case tick @ TimeStamped(ts, None) => tick.asInstanceOf[TimeSeriesValue[L]]
    case TimeStamped(ts, Some(Left(l))) => TimeStamped(ts, Some(l))
  }

  def drainLeft[F[_], L, R]: TimeSeriesTransducer[F, Either[L, R], R] = _.collect {
    case tick @ TimeStamped(ts, None) => tick.asInstanceOf[TimeSeriesValue[R]]
    case TimeStamped(ts, Some(Right(r))) => TimeStamped(ts, Some(r))
  }
}
