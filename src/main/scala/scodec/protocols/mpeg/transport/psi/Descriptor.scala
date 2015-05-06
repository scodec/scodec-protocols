package scodec.protocols.mpeg
package transport
package psi

import scodec._
import scodec.bits._
import scodec.codecs._

sealed trait KnownDescriptor

sealed trait TransportStreamDescriptor extends KnownDescriptor
sealed trait ProgramStreamDescriptor extends KnownDescriptor

case class Mpeg1Only(profileAndLevelIndication: Int, chromaFormat: Int, frameRateExtensionFlag: Boolean)
object Mpeg1Only {
  implicit val codec: Codec[Mpeg1Only] = {
    ("profile_and_level_indication" | uint8) ::
    ("chroma_format" | uint(2)) ::
    ("frame_rate_extension_flag" | bool) ::
    ("reserved" | ignore(5))
  }.dropUnits.as[Mpeg1Only]
}
case class VideoStreamDescriptor(length: Int,
  multipleFrameRateFlag: Boolean,
  frameRateCode: Int,
  mpeg1OnlyFlag: Boolean,
  constrainedParameter: Boolean,
  stillPictureFlag: Boolean,
  mpeg1Only: Option[Mpeg1Only]) extends TransportStreamDescriptor with ProgramStreamDescriptor

object VideoStreamDescriptor {
  implicit val codec: Codec[VideoStreamDescriptor] = {
    ("descriptor_length" | uint8) ::
    ("multiple_frame_rate_flag" | bool) ::
    ("frame_rate_code" | uint4) ::
    (("MPEG_1_only_flag" | bool) >>:~ { mpeg1Only =>
      ("constrained_parameter" | bool) ::
      ("still_picture_flag" | bool) ::
      ("MPEG_1_only_attributes" | conditional(mpeg1Only, Codec[Mpeg1Only]))
    })
  }.as[VideoStreamDescriptor]
}

case class AudioStreamDescriptor(length: Int, freeFormatFlag: Boolean, id: Boolean, layer: Int, variableRateAudioIndicator: Boolean) extends TransportStreamDescriptor with ProgramStreamDescriptor
object AudioStreamDescriptor {
  implicit val codec: Codec[AudioStreamDescriptor] = {
    ("descriptor_length" | uint8) ::
    ("free_format_flag" | bool) ::
    ("ID" | bool) ::
    ("layer" | uint(2)) ::
    ("variable_rate_audio_indicator" | bool) ::
    ("reserved" | ignore(3))
  }.dropUnits.as[AudioStreamDescriptor]
}

sealed trait HierarchyType
object HierarchyType {
  case object SpatialScalability extends HierarchyType
  case object SnrScalability extends HierarchyType
  case object TemporalScalability extends HierarchyType
  case object DataPartitioning extends HierarchyType
  case object ExtensionBitstream extends HierarchyType
  case object PrivateStream extends HierarchyType
  case object MultiViewProfile extends HierarchyType
  case class Reserved(value: Int) extends HierarchyType
  case object BaseLayer extends HierarchyType

  implicit val codec: Codec[HierarchyType] = {
    val m = discriminated[HierarchyType].by(uint4)
      .typecase(0, provide(Reserved(0)))
      .typecase(1, provide(SpatialScalability))
      .typecase(2, provide(SnrScalability))
      .typecase(3, provide(TemporalScalability))
      .typecase(4, provide(DataPartitioning))
      .typecase(5, provide(ExtensionBitstream))
      .typecase(6, provide(PrivateStream))
      .typecase(7, provide(MultiViewProfile))
      .typecase(15, provide(BaseLayer))
      (8 to 14).foldLeft(m) { (acc, x) => acc.subcaseP(x)({ case Reserved(y) if x == y => Reserved(y) })(provide(Reserved(x))) }
  }
}
case class HierarchyDescriptor(length: Int, hierarchyType: HierarchyType, hierarchyLayerIndex: Int, hierarchyEmbeddedLayerIndex: Int, hierarchyChannel: Int) extends TransportStreamDescriptor with ProgramStreamDescriptor
object HierarchyDescriptor {
  implicit val codec: Codec[HierarchyDescriptor] = {
    ("descriptor_length" | uint8) ::
    ("reserved" | ignore(4)) ::
    ("hierarchy_type" | Codec[HierarchyType]) ::
    ("reserved" | ignore(2)) ::
    ("hierarchy_layer_index" | uint(6)) ::
    ("reserved" | ignore(2)) ::
    ("hierarchy_embedded_layer_index" | uint(6)) ::
    ("reserved" | ignore(2)) ::
    ("hierarchy_channel" | uint(6))
  }.dropUnits.as[HierarchyDescriptor]
}

case class RegistrationDescriptor(length: Int, formatIdentifier: ByteVector, additionalIdentificationInfo: ByteVector ) extends TransportStreamDescriptor with ProgramStreamDescriptor
object RegistrationDescriptor {
  implicit val codec: Codec[RegistrationDescriptor] = {
    ("descriptor_length" | uint8) >>:~ { length =>
      (("format_identifier" | bytes(4)) ::
      ("additional_identification_info" | bytes(length - 4)))
    }
  }.as[RegistrationDescriptor]
}

sealed trait AlignmentType
object AlignmentType {
  case object SliceOrVideoAccessUnit extends AlignmentType
  case object VideoAccessUnit extends AlignmentType
  case object GopOrSeq extends AlignmentType
  case object Seq extends AlignmentType
  case class Reserved(value: Int) extends AlignmentType
  implicit val codec: Codec[AlignmentType] = {
    val m = discriminated[AlignmentType].by(uint8)
      .typecase(0, provide(Reserved(0)))
      .typecase(1, provide(SliceOrVideoAccessUnit))
      .typecase(2, provide(VideoAccessUnit))
      .typecase(3, provide(GopOrSeq))
      .typecase(4, provide(Seq))
      (5 to 255).foldLeft(m) { (acc, x) => acc.subcaseP(x)({ case Reserved(y) if x == y => Reserved(y) })(provide(Reserved(x))) }
  }
}
case class DataStreamAlignmentDescriptor(length: Int, alignmentType: AlignmentType) extends TransportStreamDescriptor with ProgramStreamDescriptor
object DataStreamAlignmentDescriptor {
  implicit val codec: Codec[DataStreamAlignmentDescriptor] = {
    ("descriptor_length" | uint8) ::
    ("alignment_type" | Codec[AlignmentType])
  }.as[DataStreamAlignmentDescriptor]
}

case class TargetBackgroundGridDescriptor(length: Int, horizontalSize: Int, verticalSize: Int, aspectRatioInformation: Int) extends TransportStreamDescriptor with ProgramStreamDescriptor
object TargetBackgroundGridDescriptor {
  implicit val codec: Codec[TargetBackgroundGridDescriptor] = {
    ("descriptor_length" | uint8) ::
    ("horizontal_size" | uint(14)) ::
    ("vertical_size" | uint(14)) ::
    ("aspect_ratio_information" | uint4)
  }.as[TargetBackgroundGridDescriptor]
}

case class VideoWindowDescriptor(length: Int, horizontalOffset: Int, verticalOffset: Int, windowPriority: Int) extends TransportStreamDescriptor with ProgramStreamDescriptor
object VideoWindowDescriptor {
  implicit val codec: Codec[VideoWindowDescriptor] = {
    ("descriptor_length" | uint8) ::
    ("horizontal_offset" | uint(14)) ::
    ("vertical_offset" | uint(14)) ::
    ("window_priority" | uint4)
  }.as[VideoWindowDescriptor]
}

case class CADescriptor(length: Int, caSystemId: Int, caPid: Pid, privateData: ByteVector) extends TransportStreamDescriptor with ProgramStreamDescriptor
object CADescriptor {
  implicit val codec: Codec[CADescriptor] = {
    (("descriptor_length" | uint8 ) >>:~ { length =>
      ("CA_system_id" | uint16) ::
      ("reserved" | ignore(3)) ::
      ("CA_PID" | Codec[Pid]) ::
      ("private_data" | bytes(length - 4))
    })
  }.dropUnits.as[CADescriptor]
}

sealed trait AudioType
object AudioType {
  case object Undefined extends AudioType
  case object CleanEffects extends AudioType
  case object HearingImpaired extends AudioType
  case object VisualImpairedCommentary extends AudioType
  case class Reserved(value: Int) extends AudioType

  implicit val codec: Codec[AudioType] = {
    val m = discriminated[AudioType].by(uint8)
      .typecase(0, provide(Undefined))
      .typecase(1, provide(CleanEffects))
      .typecase(2, provide(HearingImpaired))
      .typecase(3, provide(VisualImpairedCommentary))
    (4 to 255).foldLeft(m) { (acc, x) => acc.subcaseP(x)({ case Reserved(y) if x == y => Reserved(y) })(provide(Reserved(x))) }
  }
}

case class LanguageField(iso639LanguageCode: String, audioType: AudioType)
object LanguageField {
  implicit val codec: Codec[LanguageField] = {
    ("ISO_639_language_code" | fixedSizeBytes(3, ascii)) ::
    ("audio_type" | Codec[AudioType])
  }.as[LanguageField]
}

case class Iso639LanguageDescriptor(length: Int, languageFields: Vector[LanguageField]) extends TransportStreamDescriptor with ProgramStreamDescriptor
object Iso639LanguageDescriptor {
  implicit val codec: Codec[Iso639LanguageDescriptor] = {
    (("descriptor_length" | uint8 ) >>:~ { length =>
      ("language" | vectorOfN(provide(length / 4), Codec[LanguageField])).hlist
    })
  }.as[Iso639LanguageDescriptor]
}


case class SystemClockDescriptor(length: Int, externalClockReferenceIndicator: Boolean, clockAccuracyInteger: Int, clockAccuracyExponent: Int) extends TransportStreamDescriptor with ProgramStreamDescriptor
object SystemClockDescriptor {
  implicit val codec: Codec[SystemClockDescriptor] = {
    ("descriptor_length" | uint8 ) ::
    ("external_clock_reference_indicator" | bool) ::
    ("reserved" | ignore(1)) ::
    ("clock_accuracy_integer" | uint(6)) ::
    ("clock_accuracy_exponent" | uint(3)) ::
    ("reserved" | ignore(5))
  }.dropUnits.as[SystemClockDescriptor]
}

case class MultiplexBufferUtilizationDescriptor(length: Int, boundValidFlag: Boolean, ltwOffsetLowerBound: Int, ltwOffsetUpperBound: Int) extends TransportStreamDescriptor with ProgramStreamDescriptor
object MultiplexBufferUtilizationDescriptor {
  implicit val codec: Codec[MultiplexBufferUtilizationDescriptor] = {
    ("descriptor_length" | uint8 ) ::
    ("bound_valid_flag" | bool) ::
    ("LTW_offset_lower_bound" | uint(15)) ::
    ("reserved" | ignore(1)) ::
    ("LTW_offset_upper_bound" | uint(14))
  }.dropUnits.as[MultiplexBufferUtilizationDescriptor]
}

case class CopyrightDescriptor(length: Int, copyrightIdentifier: ByteVector, additionalCopyrightInfo: ByteVector) extends TransportStreamDescriptor with ProgramStreamDescriptor
object CopyrightDescriptor {
  implicit val codec: Codec[CopyrightDescriptor] = {
    ("descriptor_length" | uint8) >>:~ { length =>
      (("copyright_identifier" | bytes(4)) ::
      ("additional_copyright_info" | bytes(length - 4)))
    }
  }.as[CopyrightDescriptor]
}

case class MaximumBitrateDescriptor(length: Int, maximumBitrate: Int) extends TransportStreamDescriptor
object MaximumBitrateDescriptor {
  implicit val codec: Codec[MaximumBitrateDescriptor] = {
    ("descriptor_length" | uint8 ) ::
    ("reserved" | ignore(2)) ::
    ("maximum_bitrate" | uint(22))
  }.dropUnits.as[MaximumBitrateDescriptor]
}

case class PrivateDataIndicatorDescriptor(length: Int, privateDataIndicator: ByteVector) extends TransportStreamDescriptor with ProgramStreamDescriptor
object PrivateDataIndicatorDescriptor {
  implicit val codec: Codec[PrivateDataIndicatorDescriptor] = {
    ("descriptor_length" | uint8 ) ::
    ("private_data_indicator" | bytes(4))
  }.as[PrivateDataIndicatorDescriptor]
}

case class SmoothingBufferDescriptor(length: Int, sbLeakRate: Int, sbSize: Int) extends TransportStreamDescriptor with ProgramStreamDescriptor
object SmoothingBufferDescriptor {
  implicit val codec: Codec[SmoothingBufferDescriptor] = {
    ("descriptor_length" | uint8 ) ::
    ("reserved" | ignore(2)) ::
    ("sb_leak_rate" | uint(22)) ::
    ("reserved" | ignore(2)) ::
    ("sb_size" | uint(22))
  }.dropUnits.as[SmoothingBufferDescriptor]
}

case class StdDescriptor(length: Int, leakValidFlag: Boolean) extends TransportStreamDescriptor
object StdDescriptor {
  implicit val codec: Codec[StdDescriptor] = {
    ("descriptor_length" | uint8 ) ::
    ("reserved" | ignore(7)) ::
    ("leak_valid_flag" | bool)
  }.dropUnits.as[StdDescriptor]
}

case class IbpDescriptor(length: Int, closedGopFlag: Boolean, identicalGopFlag: Boolean, maxGopLength: Int) extends TransportStreamDescriptor with ProgramStreamDescriptor
object IbpDescriptor {
  implicit val codec: Codec[IbpDescriptor] = {
    ("descriptor_length" | uint8 ) ::
    ("closed_gop_flag" | bool) ::
    ("identical_gop_flag" | bool) ::
    ("max_gop_length" | uint(14))
  }.as[IbpDescriptor]
}

case class Mpeg4VideoDescriptor(length: Int, mpeg4VisualProfileAndLevel: Byte) extends TransportStreamDescriptor with ProgramStreamDescriptor
object Mpeg4VideoDescriptor {
  implicit val codec: Codec[Mpeg4VideoDescriptor] = {
    ("descriptor_length" | uint8 ) ::
    ("MPEG-4_visual_profile_and_level" | byte)
  }.as[Mpeg4VideoDescriptor]
}

case class Mpeg4AudioDescriptor(length: Int, mpeg4AudioProfileAndLevel: Byte) extends TransportStreamDescriptor with ProgramStreamDescriptor
object Mpeg4AudioDescriptor {
  implicit val codec: Codec[Mpeg4AudioDescriptor] = {
    ("descriptor_length" | uint8 ) ::
    ("MPEG-4_audio_profile_and_level" | byte)
  }.as[Mpeg4AudioDescriptor]
}

case class IodDescriptor(length: Int, scopeOfIodLabel: Byte, iodLabel: Byte, initialObjectDescriptor: Byte) extends TransportStreamDescriptor with ProgramStreamDescriptor
object IodDescriptor {
  implicit val codec: Codec[IodDescriptor] = {
    ("descriptor_length" | uint8) ::
    ("Scope_of_IOD_label" | byte) ::
    ("IOD_label" | byte) ::
    ("initialObjectDescriptor" | byte)
  }.as[IodDescriptor]
}

case class SlDescriptor(length: Int, esId: Int) extends TransportStreamDescriptor
object SlDescriptor {
  implicit val codec: Codec[SlDescriptor] = {
    ("descriptor_length" | uint8 ) ::
    ("ES_ID" | uint16)
  }.as[SlDescriptor]
}

case class EsIdAndChannel(esId: Int, flexMuxChannel: Int)
object EsIdAndChannel {
  implicit val codec: Codec[EsIdAndChannel] = {
    ("ES_ID" | uint16) ::
    ("FlexMuxChannel" | uint8)
  }.as[EsIdAndChannel]
}
case class FmcDescriptor(length: Int, channels: Vector[EsIdAndChannel]) extends TransportStreamDescriptor with ProgramStreamDescriptor
object FmcDescriptor {
  implicit val codec: Codec[FmcDescriptor] = {
    (("descriptor_length" | uint8 ) >>:~ { length =>
      ("channels" | vectorOfN(provide(length / 3), Codec[EsIdAndChannel])).hlist
    })
  }.as[FmcDescriptor]
}

case class ExternalEsIdDescriptor(length: Int, esternalEsId: Int) extends TransportStreamDescriptor with ProgramStreamDescriptor
object ExternalEsIdDescriptor {
  implicit val codec: Codec[ExternalEsIdDescriptor] = {
    ("descriptor_length" | uint8 ) ::
    ("External_ES_ID" | uint16)
  }.as[ExternalEsIdDescriptor]
}

case class MuxCodeDescriptor(length: Int, muxCodeTableEntry: ByteVector) extends TransportStreamDescriptor with ProgramStreamDescriptor
object MuxCodeDescriptor {
  implicit val codec: Codec[MuxCodeDescriptor] = {
    (("descriptor_length" | uint8) >>:~ { length =>
      ("MuxCodeTableEntry" | bytes(length)).hlist
    })
  }.as[MuxCodeDescriptor]
}

case class FmxBufferSizeDescriptor(length: Int, flexMuxBufferDescriptor: ByteVector) extends TransportStreamDescriptor with ProgramStreamDescriptor
object FmxBufferSizeDescriptor {
  implicit val codec: Codec[FmxBufferSizeDescriptor] = {
    (("descriptor_length" | uint8) >>:~ { length =>
      ("FlexMuxBufferDescriptor" | bytes(length)).hlist
    })
  }.as[FmxBufferSizeDescriptor]
}

case class MultiplexBufferDescriptor(length: Int, mbBufferSize: Int, tbLeakRate: Int) extends TransportStreamDescriptor with ProgramStreamDescriptor
object MultiplexBufferDescriptor {
  implicit val codec: Codec[MultiplexBufferDescriptor] = {
    ("descriptor_length" | uint8) ::
    ("MB_buffer_size" | uint24) ::
    ("TB_leak_rate" | uint24)
  }.as[MultiplexBufferDescriptor]
}

case class UnknownDescriptor(tag: Int, length: Int, data: ByteVector)
object UnknownDescriptor {
  implicit val codec: Codec[UnknownDescriptor] = {
    ("descriptor_tag" | uint8) ::
    (("descriptor_length" | uint8) >>:~ { length =>
      ("descriptor_data" | bytes(length)).hlist
    })
  }.as[UnknownDescriptor]
}

object Descriptor {
  type Descriptor = Either[UnknownDescriptor, KnownDescriptor]

  // Using typecase instead of implicit discriminators per type for fast compilation
  implicit val knownCodec: Codec[KnownDescriptor] = discriminated[KnownDescriptor].by(uint8)
    .typecase(2, Codec[VideoStreamDescriptor])
    .typecase(3, Codec[AudioStreamDescriptor])
    .typecase(4, Codec[HierarchyDescriptor])
    .typecase(5, Codec[RegistrationDescriptor])
    .typecase(6, Codec[DataStreamAlignmentDescriptor])
    .typecase(7, Codec[TargetBackgroundGridDescriptor])
    .typecase(8, Codec[VideoWindowDescriptor])
    .typecase(9, Codec[CADescriptor])
    .typecase(10, Codec[Iso639LanguageDescriptor])
    .typecase(11, Codec[SystemClockDescriptor])
    .typecase(12, Codec[MultiplexBufferUtilizationDescriptor])
    .typecase(13, Codec[CopyrightDescriptor])
    .typecase(14, Codec[MaximumBitrateDescriptor])
    .typecase(15, Codec[PrivateDataIndicatorDescriptor])
    .typecase(16, Codec[SmoothingBufferDescriptor])
    .typecase(17, Codec[StdDescriptor])
    .typecase(18, Codec[IbpDescriptor])
    .typecase(27, Codec[Mpeg4VideoDescriptor])
    .typecase(28, Codec[Mpeg4AudioDescriptor])
    .typecase(29, Codec[IodDescriptor])
    .typecase(30, Codec[SlDescriptor])
    .typecase(31, Codec[FmcDescriptor])
    .typecase(32, Codec[ExternalEsIdDescriptor])
    .typecase(33, Codec[MuxCodeDescriptor])
    .typecase(34, Codec[FmxBufferSizeDescriptor])
    .typecase(35, Codec[MultiplexBufferDescriptor])

  implicit val codec: Codec[Descriptor] = discriminatorFallbackCodec[UnknownDescriptor, KnownDescriptor]

  private def discriminatorFallbackCodec[U, K](implicit unknownCodec: Codec[U], knownCodec: Codec[K]): Codec[Either[U, K]] =
    new Codec[Either[U, K]] {
      def encode(value: Either[U, K]): Attempt[BitVector] = value.fold(unknown => unknownCodec.encode(unknown),
        known => knownCodec.encode(known))
      def decode(bits: BitVector): Attempt[DecodeResult[Either[U, K]]] =
        knownCodec.decode(bits).fold({
          case _: KnownDiscriminatorType[_]#UnknownDiscriminator => unknownCodec.decode(bits).map { _.map { d => Left(d) } }
          case err => Attempt.failure(err)
        }, result => Attempt.successful(result.map { k => Right(k) }))
      def sizeBound: SizeBound = SizeBound.unknown
    }
}
