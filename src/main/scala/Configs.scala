package dma

import config.{Parameters, Config}
import unittest.UnitTests
import rocketchip.BaseConfig

class WithDma extends Config((site, here, up) => {
  case DmaKey => DmaConfig()
})

class WithDmaUnitTests extends Config((site, here, up) => {
  case UnitTests => (p: Parameters) => DmaUnitTests(p)
})

class DmaUnitTestConfig extends Config(
  new WithDma ++ new WithDmaUnitTests ++ new BaseConfig)
