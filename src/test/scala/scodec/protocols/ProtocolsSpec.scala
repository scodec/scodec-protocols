package scodec.protocols

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

abstract class ProtocolsSpec
  extends AnyWordSpec
  with Matchers
  with ScalaCheckDrivenPropertyChecks
