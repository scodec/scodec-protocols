package scodec.protocols

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

abstract class ProtocolsSpec
  extends AnyWordSpec
  with Matchers
  with GeneratorDrivenPropertyChecks
