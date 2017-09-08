package dma

import freechips.rocketchip.config.Parameters
import testchipip.GeneratorApp

class TestHarness(implicit p: Parameters)
    extends freechips.rocketchip.unittest.TestHarness

object Generator extends GeneratorApp {
  generateFirrtl
}
