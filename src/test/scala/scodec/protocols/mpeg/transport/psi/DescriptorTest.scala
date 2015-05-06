package scodec.protocols
package mpeg
package transport
package psi

import Descriptor._

import scodec._
import scodec.bits._

import org.scalacheck.{ Arbitrary, Gen }

class DescriptorTest extends ProtocolsSpec {
  import DescriptorTestData._

  "the Descriptor class" should {
    "support relevant descriptors" which {
      "handles decoding and encoding for valid values" in {
        forAll { d: Descriptor => roundtrip(Codec[Descriptor], d) }
      }
    }
  }
  private def roundtrip[A](codec: Codec[A], value: A): Unit = {
    val encoded = codec.encode(value)
    encoded shouldBe 'successful
    val Attempt.Successful(DecodeResult(decoded, remainder)) = codec.decode(encoded.require)
    remainder shouldEqual BitVector.empty
    decoded shouldEqual value
  }
}
object DescriptorTestData {

  def genMpeg1Only(flag: Boolean): Gen[Option[Mpeg1Only]] =
    if (!flag) None
    else
      for {
        profileAndLevelIndication <- Gen.chooseNum(0, 255)
        chromaFormat <- Gen.chooseNum(0, 3)
        frameRateExtensionFlag <- Gen.oneOf(true, false)
      } yield Some(Mpeg1Only(profileAndLevelIndication, chromaFormat, frameRateExtensionFlag))

  val genVideoStreamDescriptor: Gen[VideoStreamDescriptor] = for {
    multipleFrameRateFlag <- Gen.oneOf(true, false)
    frameRateCode <- Gen.chooseNum(0, 0xFFFFFFFF)
    mpeg1OnlyFlag <- Gen.oneOf(true, false)
    constrainedParameter <- Gen.oneOf(true, false)
    stillPictureFlag <- Gen.oneOf(true, false)
    mpeg1Only <- genMpeg1Only(mpeg1OnlyFlag)
    length = if (mpeg1OnlyFlag) 3 else 1
  } yield VideoStreamDescriptor(length, multipleFrameRateFlag, frameRateCode, mpeg1OnlyFlag, constrainedParameter, stillPictureFlag, mpeg1Only)

  val genAudioStreamDescriptor: Gen[AudioStreamDescriptor] = for {
    freeFormatFlag <- Gen.oneOf(true, false)
    id <- Gen.oneOf(true, false)
    layer <- Gen.chooseNum(0, 3)
    variableRateAudioIndicator <- Gen.oneOf(true, false)
  } yield AudioStreamDescriptor(1, freeFormatFlag, id, layer, variableRateAudioIndicator)

  val genHierarchyType: Gen[HierarchyType] = Gen.oneOf(
    HierarchyType.SpatialScalability,
    HierarchyType.SnrScalability,
    HierarchyType.TemporalScalability,
    HierarchyType.DataPartitioning,
    HierarchyType.ExtensionBitstream,
    HierarchyType.PrivateStream,
    HierarchyType.MultiViewProfile,
    HierarchyType.Reserved(0),
    HierarchyType.BaseLayer)

  val genHierarchyDescriptor: Gen[HierarchyDescriptor] = for {
    hierarchyType <- genHierarchyType
    hierarchyLayerIndex <- Gen.chooseNum(0, 63)
    hierarchyEmbeddedLayerIndex <- Gen.chooseNum(0, 63)
    hierarchyChannel <- Gen.chooseNum(0, 63)
  } yield HierarchyDescriptor(4, hierarchyType, hierarchyLayerIndex, hierarchyEmbeddedLayerIndex, hierarchyChannel)

  val genRegistrationDescriptor: Gen[RegistrationDescriptor] = for {
    length <- Gen.chooseNum(4, 255)
    formatIdentifier <- Gen.listOfN(4, Gen.chooseNum(0, 255))
    additionalIdentificationInfo <- Gen.listOfN(length - 4, Gen.chooseNum(0, 255))
  } yield RegistrationDescriptor(length, ByteVector(formatIdentifier: _*), ByteVector(additionalIdentificationInfo: _*))

  val genDataStreamAlignmentDescriptor: Gen[DataStreamAlignmentDescriptor] = for {
    alignmentType <- Gen.oneOf(AlignmentType.Reserved(0),
      AlignmentType.SliceOrVideoAccessUnit,
      AlignmentType.VideoAccessUnit,
      AlignmentType.GopOrSeq,
      AlignmentType.Seq)
  } yield DataStreamAlignmentDescriptor(1, alignmentType)

  val genTargetBackgroundGridDescriptor: Gen[TargetBackgroundGridDescriptor] = for {
    horizontalSize <- Gen.chooseNum(0, 16383)
    verticalSize <- Gen.chooseNum(0, 16383)
    aspectRatioInformation <- Gen.choose(0, 15)
  } yield TargetBackgroundGridDescriptor(4, horizontalSize, verticalSize, aspectRatioInformation)

  val genVideoWindowDescriptor: Gen[VideoWindowDescriptor] = for {
    horizontalOffset <- Gen.chooseNum(0, 16383)
    verticalOffset <- Gen.chooseNum(0, 16383)
    windowPriority <- Gen.choose(0, 15)
  } yield VideoWindowDescriptor(4, horizontalOffset, verticalOffset, windowPriority)

  val genCADescriptor: Gen[CADescriptor] = for {
    length <- Gen.chooseNum(4, 255)
    caSystemId <- Gen.chooseNum(0, 65535)
    caPid <- Gen.choose(0, 8191)
    privateData <- Gen.listOfN(length - 4, Gen.chooseNum(0, 255))
  } yield CADescriptor(length, caSystemId, Pid(caPid), ByteVector(privateData: _*))

  val genLanguageField: Gen[LanguageField] = for {
    iso639LanguageCode  <- Gen.listOfN(3, Gen.alphaChar)
    audioType <- Gen.oneOf(AudioType.Undefined, AudioType.CleanEffects, AudioType.HearingImpaired, AudioType.VisualImpairedCommentary, AudioType.Reserved(4))
  } yield LanguageField(iso639LanguageCode.mkString, audioType)

  val genIso639LanguageDescriptor: Gen[Iso639LanguageDescriptor] = for {
    languageFields <- Gen.listOf(genLanguageField)
    length = languageFields.size * 4
  } yield Iso639LanguageDescriptor(length, languageFields.toVector)

  val genSystemClockDescriptor: Gen[SystemClockDescriptor] = for {
     externalClockReferenceIndicator <- Gen.oneOf(true, false)
     clockAccuracyInteger <- Gen.oneOf(0, 63)
     clockAccuracyExponent <- Gen.oneOf(0, 7)
  } yield SystemClockDescriptor(2, externalClockReferenceIndicator, clockAccuracyInteger, clockAccuracyExponent)

  val genMultiplexBufferUtilizationDescriptor: Gen[MultiplexBufferUtilizationDescriptor] = for {
    boundValidFlag <- Gen.oneOf(true, false)
    ltwOffsetLowerBound <- Gen.oneOf(0, 32767)
    ltwOffsetUpperBound <- Gen.oneOf(0, 16383)
  } yield MultiplexBufferUtilizationDescriptor(4, boundValidFlag, ltwOffsetLowerBound, ltwOffsetUpperBound)

  val genCopyrightDescriptor: Gen[CopyrightDescriptor] = for {
    length <- Gen.chooseNum(4, 255)
    copyrightIdentifier <- Gen.listOfN(4, Gen.chooseNum(0, 255))
    additionalCopyrightInfo <- Gen.listOfN(length - 4, Gen.chooseNum(0, 255))
  } yield CopyrightDescriptor(length, ByteVector(copyrightIdentifier: _*), ByteVector(additionalCopyrightInfo: _*))

  val genMaximumBitrateDescriptor: Gen[MaximumBitrateDescriptor] = for {
    maximumBitrate <- Gen.chooseNum(0, 4194303)
  } yield MaximumBitrateDescriptor(3, maximumBitrate)

  val genPrivateDataIndicatorDescriptor: Gen[PrivateDataIndicatorDescriptor] = for {
    privateDataIndicator <- Gen.listOfN(4, Gen.chooseNum(0, 255))
  } yield PrivateDataIndicatorDescriptor(4, ByteVector(privateDataIndicator: _*))

  val genSmoothingBufferDescriptor: Gen[SmoothingBufferDescriptor] = for {
    sbLeakRate <- Gen.chooseNum(0, 4194303)
    sbSize <- Gen.chooseNum(0, 4194303)
  } yield SmoothingBufferDescriptor(6, sbLeakRate, sbSize)

  val genStdDescriptor: Gen[StdDescriptor] =
    for { leakValidFlag <- Gen.oneOf(true, false) } yield StdDescriptor(1, leakValidFlag)

  val genIbpDescriptor: Gen[IbpDescriptor] = for {
   closedGopFlag <- Gen.oneOf(true, false)
   identicalGopFlag <- Gen.oneOf(true, false)
   maxGopLength <- Gen.chooseNum(0, 16383)
  } yield IbpDescriptor(2, closedGopFlag, identicalGopFlag, maxGopLength)

  val genMpeg4VideoDescriptor: Gen[Mpeg4VideoDescriptor] =
    for { mpeg4VisualProfileAndLevel <- Gen.chooseNum(0, 255) } yield Mpeg4VideoDescriptor(1, mpeg4VisualProfileAndLevel.toByte)

  val genMpeg4AudioDescriptor: Gen[Mpeg4AudioDescriptor] =
    for { mpeg4AudioProfileAndLevel <- Gen.chooseNum(0, 255) } yield Mpeg4AudioDescriptor(1, mpeg4AudioProfileAndLevel.toByte)

  val genIodDescriptor: Gen[IodDescriptor] = for {
    scopeOfIodLabel <- Gen.chooseNum(0, 255)
    iodLabel <- Gen.chooseNum(0, 255)
    initialObjectDescriptor <- Gen.chooseNum(0, 255)
  } yield IodDescriptor(3, scopeOfIodLabel.toByte, iodLabel.toByte, initialObjectDescriptor.toByte)

  val genSlDescriptor: Gen[SlDescriptor] =
    for { esId <- Gen.chooseNum(0, 65535) } yield SlDescriptor(2, esId: Int)

  val genEsIdAndChannel: Gen[EsIdAndChannel] = for {
    esId <- Gen.chooseNum(0, 65535)
    flexMuxChannel <- Gen.chooseNum(0, 255)
  } yield EsIdAndChannel(esId, flexMuxChannel)

  val genFmcDescriptor: Gen[FmcDescriptor] = for {
    channels <- Gen.listOf(genEsIdAndChannel)
    length = channels.size * 3
  } yield FmcDescriptor(length, channels.toVector)

  val genExternalEsIdDescriptor: Gen[ExternalEsIdDescriptor] =
    for { externalEsId <- Gen.chooseNum(0, 65535) } yield ExternalEsIdDescriptor(2, externalEsId)

  val genMuxCodeDescriptor: Gen[MuxCodeDescriptor] = for {
    length <- Gen.chooseNum(0, 255)
    muxCodeTableEntry <- Gen.listOfN(length, Gen.chooseNum(0, 255))
  } yield MuxCodeDescriptor(length, ByteVector(muxCodeTableEntry: _*))

  val genFmxBufferSizeDescriptor: Gen[FmxBufferSizeDescriptor] = for {
    length <- Gen.chooseNum(0, 255)
    flexMuxBufferDescriptor <- Gen.listOfN(length, Gen.chooseNum(0, 255))
  } yield FmxBufferSizeDescriptor(length, ByteVector(flexMuxBufferDescriptor: _*))

  val genMultiplexBufferDescriptor: Gen[MultiplexBufferDescriptor] = for {
    mbBufferSize <- Gen.chooseNum(0, 16777215)
    tbLeakRate <- Gen.chooseNum(0, 16777215)
  } yield MultiplexBufferDescriptor(6, mbBufferSize, tbLeakRate)

  val genKnownDescriptor: Gen[KnownDescriptor] = Gen.oneOf(
    genVideoStreamDescriptor,
    genAudioStreamDescriptor,
    genHierarchyDescriptor,
    genRegistrationDescriptor,
    genDataStreamAlignmentDescriptor,
    genTargetBackgroundGridDescriptor,
    genVideoWindowDescriptor,
    genCADescriptor,
    genIso639LanguageDescriptor,
    genSystemClockDescriptor,
    genMultiplexBufferUtilizationDescriptor,
    genCopyrightDescriptor,
    genMaximumBitrateDescriptor,
    genPrivateDataIndicatorDescriptor,
    genSmoothingBufferDescriptor,
    genStdDescriptor,
    genIbpDescriptor,
    genMpeg4VideoDescriptor,
    genMpeg4AudioDescriptor,
    genIodDescriptor,
    genSlDescriptor,
    genFmcDescriptor,
    genExternalEsIdDescriptor,
    genMuxCodeDescriptor,
    genFmxBufferSizeDescriptor,
    genMultiplexBufferDescriptor)

  val genUnknownDescriptor: Gen[UnknownDescriptor] = for {
    tag <- Gen.chooseNum(36, 255)
    length <- Gen.chooseNum(0, 255)
    data <- Gen.listOfN(length, Gen.chooseNum(0, 255))
  } yield UnknownDescriptor(tag, length, ByteVector(data: _*))

  val genDescriptor: Gen[Descriptor] = Gen.oneOf(genKnownDescriptor, genUnknownDescriptor).map {
    case known: KnownDescriptor => Right(known)
    case unknown: UnknownDescriptor => Left(unknown)
  }

  implicit val arbitraryDescriptor: Arbitrary[Descriptor] = Arbitrary(genDescriptor)
}
