package dma

import freechips.rocketchip.config.{Parameters, Config}
import freechips.rocketchip.unittest.UnitTests
import freechips.rocketchip.system.BaseConfig

class WithDma extends Config((site, here, up) => {
  case DmaKey => DmaConfig()
})

class WithDmaUnitTests extends Config((site, here, up) => {
  case UnitTests => (p: Parameters) => DmaUnitTests(p)
})

class DmaUnitTestConfig extends Config(
  new WithDma ++ new WithDmaUnitTests ++ new BaseConfig)
