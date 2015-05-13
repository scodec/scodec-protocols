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
