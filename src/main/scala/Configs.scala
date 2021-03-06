package dma

import freechips.rocketchip.config.{Parameters, Config}
import freechips.rocketchip.diplomacy.{LazyModule, ValName}
import freechips.rocketchip.system.BaseConfig
import freechips.rocketchip.tile.{OpcodeSet, BuildRoCC}
import freechips.rocketchip.unittest.UnitTests

object ConfigValName {
  implicit val valName = ValName("TestHarness")
}
import ConfigValName._

class WithDma extends Config((site, here, up) => {
  case DmaKey => DmaConfig()
  case BuildRoCC => Seq((p: Parameters) =>
      LazyModule(new CopyAccelerator(OpcodeSet.custom2)(p)))
})

class WithDmaUnitTests extends Config((site, here, up) => {
  case UnitTests => (p: Parameters) => DmaUnitTests(p)
})

class DmaUnitTestConfig extends Config(
  new WithDma ++ new WithDmaUnitTests ++ new BaseConfig)
