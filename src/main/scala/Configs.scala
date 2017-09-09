package dma

import freechips.rocketchip.config.{Parameters, Config}
import freechips.rocketchip.coreplex.RocketTilesKey
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.system.BaseConfig
import freechips.rocketchip.tile.{RoCCParams, OpcodeSet}
import freechips.rocketchip.unittest.UnitTests

class WithDma extends Config((site, here, up) => {
  case DmaKey => DmaConfig()
  case RocketTilesKey => up(RocketTilesKey, site) map { r =>
    r.copy(rocc = Seq(
      RoCCParams(
        opcodes = OpcodeSet.custom2,
        generator = (p: Parameters) => LazyModule(new CopyAccelerator()(p)),
        nPTWPorts = 1)))
  }
})

class WithDmaUnitTests extends Config((site, here, up) => {
  case UnitTests => (p: Parameters) => DmaUnitTests(p)
})

class DmaUnitTestConfig extends Config(
  new WithDma ++ new WithDmaUnitTests ++ new BaseConfig)
