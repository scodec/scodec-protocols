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

package scodec.protocols.mpeg

import scodec.bits._

object PesStreamId {
  val ProgramStreamMap =               bin"1011 1100".toInt(signed = false)
  val PrivateStream1 =                 bin"1011 1101".toInt(signed = false)
  val PaddingStream =                  bin"1011 1110".toInt(signed = false)
  val PrivateStream2 =                 bin"1011 1111".toInt(signed = false)
  val AudioStreamMin =                 bin"1100 0000".toInt(signed = false)
  val AudioStreamMax =                 bin"1101 1111".toInt(signed = false)
  val VideoStreamMin =                 bin"1110 0000".toInt(signed = false)
  val VideoStreamMax =                 bin"1110 1111".toInt(signed = false)
  val ECM =                            bin"1111 0000".toInt(signed = false)
  val EMM =                            bin"1111 0001".toInt(signed = false)
  val DSMCC =                          bin"1111 0010".toInt(signed = false)
  val `ISO/IEC 13522` =                bin"1111 0011".toInt(signed = false)
  val `ITU-T Rec. H.222.1 type A` =    bin"1111 0100".toInt(signed = false)
  val `ITU-T Rec. H.222.1 type B` =    bin"1111 0101".toInt(signed = false)
  val `ITU-T Rec. H.222.1 type C` =    bin"1111 0110".toInt(signed = false)
  val `ITU-T Rec. H.222.1 type D` =    bin"1111 0111".toInt(signed = false)
  val `ITU-T Rec. H.222.1 type E` =    bin"1111 1000".toInt(signed = false)
  val Ancillary =                      bin"1111 1001".toInt(signed = false)
  val `ISO/IEC14496-1 SL Packetized` = bin"1111 1010".toInt(signed = false)
  val `ISO/IEC14496-1 FlexMux` =       bin"1111 1011".toInt(signed = false)
  val ReservedMin =                    bin"1111 1100".toInt(signed = false)
  val ReservedMax =                    bin"1111 1110".toInt(signed = false)
  val ProgramStreamDirectory =         bin"1111 1111".toInt(signed = false)
}
