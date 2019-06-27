package scodec.protocols

import org.scalatest.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

abstract class ProtocolsSpec
  extends AnyWordSpec
  with Matchers
  with ScalaCheckDrivenPropertyChecks
