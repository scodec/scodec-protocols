package scodec.protocols

import org.scalatest.{ WordSpec, Matchers }
import org.scalatest.prop.GeneratorDrivenPropertyChecks

abstract class ProtocolsSpec
  extends WordSpec
  with Matchers
  with GeneratorDrivenPropertyChecks
