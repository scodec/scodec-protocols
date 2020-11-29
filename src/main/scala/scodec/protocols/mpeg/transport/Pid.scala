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
package mpeg
package transport

import scodec.Codec
import scodec.codecs.uint

case class Pid(value: Int) {
  require(value >= Pid.MinValue && value <= Pid.MaxValue)
}

object Pid {
  val MinValue = 0
  val MaxValue = 8191

  implicit val codec: Codec[Pid] = uint(13).as[Pid]
}

case class PidStamped[+A](pid: Pid, value: A) {
  def map[B](f: A => B): PidStamped[B] = copy(value = f(value))
}

object PidStamped {

  /**
   * Combinator that converts a `Transform[I, O]` in to a `Transform[PidStamped[I], PidStamped[O]]` such that
   * pidstamps are preserved on elements that flow through the stream.
   */
  def preserve[I, O](t: Transform[I, O]): Transform.Aux[t.S, PidStamped[I], PidStamped[O]] =
    t.lens(_.value, (psi, o) => psi.copy(value = o))
}
