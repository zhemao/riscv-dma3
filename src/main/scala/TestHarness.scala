package dma

import config.Parameters
import testchipip.GeneratorApp

class TestHarness(implicit p: Parameters) extends unittest.TestHarness

object Generator extends GeneratorApp {
  generateFirrtl
}
