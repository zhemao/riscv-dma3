package dma

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile.{LazyRoCC, LazyRoCCModule, RoCCCommand, RoCCResponse}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.config.{Parameters, Field}
import scala.math.max

object DmaCtrlRegNumbers {
  val SRC_STRIDE = 0
  val DST_STRIDE = 1
  val SEGMENT_SIZE = 2
  val NSEGMENTS = 3
  val ACCEL_CTRL = 4
  val RESP_STATUS = 5
  val RESP_VPN = 6
}
import DmaCtrlRegNumbers._

class DmaCtrlRegFile(implicit val p: Parameters) extends Module
    with HasClientDmaParameters {

  private val nRegs = 7

  val io = IO(new Bundle {
    val wen = Input(Bool())
    val rwaddr = Input(UInt(log2Up(nRegs).W))
    val wdata = Input(UInt(dmaSegmentSizeBits.W))
    val rdata = Output(UInt(dmaSegmentSizeBits.W))
    val set = Input(Bool())
    val clear = Input(Bool())

    val src_stride = Output(UInt(dmaSegmentSizeBits.W))
    val dst_stride = Output(UInt(dmaSegmentSizeBits.W))
    val segment_size = Output(UInt(dmaSegmentSizeBits.W))
    val nsegments = Output(UInt(dmaSegmentBits.W))
    val alloc = Output(UInt(2.W))
    val pause = Output(Bool())

    val dma_resp = Flipped(Valid(new ClientDmaResponse))
    //val sg_resp = Flipped(Valid(new ScatterGatherResponse))
    val error = Output(Bool())
  })

  val regSize = max(dmaSegmentSizeBits, vpnBits)
  val regs = Reg(Vec(nRegs, UInt(regSize.W)))

  when (reset) {
    regs(ACCEL_CTRL) := "b010".U
    regs(RESP_STATUS) := 0.U
  }

  io.src_stride := regs(SRC_STRIDE)
  io.dst_stride := regs(DST_STRIDE)
  io.segment_size := regs(SEGMENT_SIZE)
  io.nsegments := regs(NSEGMENTS)
  io.alloc := regs(ACCEL_CTRL)(1, 0)
  io.pause := regs(ACCEL_CTRL)(2)

  val wdata = MuxCase(io.wdata, Seq(
    io.set -> (regs(io.rwaddr) | io.wdata),
    io.clear -> (regs(io.rwaddr) & ~io.wdata)))

  when (io.wen) { regs(io.rwaddr) := wdata }
  when (io.dma_resp.valid) {
    regs(RESP_STATUS) := io.dma_resp.bits.status
    regs(RESP_VPN) := io.dma_resp.bits.fault_vpn
  }
  //when (io.sg_resp.valid) {
  //  regs(RESP_STATUS) := io.sg_resp.bits.status
  //  regs(RESP_VPN) := io.sg_resp.bits.fault_vpn
  //}

  io.rdata := regs(io.rwaddr)
  io.error := regs(RESP_STATUS) =/= 0.U
}

class DmaController(implicit val p: Parameters, edge: TLEdgeOut) extends Module
    with HasClientDmaParameters {
  val io = IO(new Bundle {
    val cmd = Flipped(Decoupled(new RoCCCommand))
    val resp = Decoupled(new RoCCResponse)
    val ptw = new TLBPTWIO
    val dma = new DmaIO
    val mem = new HellaCacheIO
    val busy = Output(Bool())
    val interrupt = Output(Bool())
  })

  val cmd = Queue(io.cmd)
  val inst = cmd.bits.inst
  val is_transfer = inst.funct < 4.U
  val is_cr_read = inst.funct === 4.U
  val is_cr_write = inst.funct >= 5.U && inst.funct <= 7.U
  val is_cr_set = inst.funct === 6.U
  val is_cr_clear = inst.funct === 7.U
  val is_sg = inst.funct >= 8.U

  val crfile = Module(new DmaCtrlRegFile)
  val frontend = Module(new DmaFrontend)
  //val sgunit = Module(new ScatterGatherUnit(1))
  val clientArb = Module(new ClientDmaArbiter(1))

  crfile.io.rwaddr := cmd.bits.rs1
  crfile.io.wdata := cmd.bits.rs2
  crfile.io.wen := cmd.fire() && is_cr_write
  crfile.io.set := is_cr_set
  crfile.io.clear := is_cr_clear
  crfile.io.dma_resp <> clientArb.io.in(0).resp
  //crfile.io.sg_resp <> sgunit.io.cpu.resp

  clientArb.io.in(0).req.valid := cmd.valid && is_transfer
  clientArb.io.in(0).req.bits := ClientDmaRequest(
    client_id = 0.U,
    cmd = cmd.bits.inst.funct,
    src_start = cmd.bits.rs2,
    dst_start = cmd.bits.rs1,
    src_stride = crfile.io.src_stride,
    dst_stride = crfile.io.dst_stride,
    segment_size = crfile.io.segment_size,
    nsegments = crfile.io.nsegments,
    alloc = crfile.io.alloc)
  //clientArb.io.in(1) <> sgunit.io.dma

  frontend.io.cpu <> clientArb.io.out
  frontend.io.pause := crfile.io.pause

  val status = RegEnable(cmd.bits.status, cmd.fire() && (is_transfer || is_sg))
  val tlb = Module(new FrontendTLB(1))
  tlb.io.clients(0) <> frontend.io.tlb
  //tlb.io.clients(1) <> sgunit.io.tlb
  io.ptw <> tlb.io.ptw
  tlb.io.ptw.status := status

  //sgunit.io.cpu.req.valid := cmd.valid && is_sg
  //sgunit.io.cpu.req.bits := ScatterGatherRequest(
  //  cmd = cmd.bits.inst.funct,
  //  src_start = cmd.bits.rs2,
  //  dst_start = cmd.bits.rs1,
  //  segment_size = crfile.io.segment_size,
  //  nsegments = crfile.io.nsegments,
  //  alloc = crfile.io.alloc)

  io.dma <> frontend.io.dma
  //io.mem <> sgunit.io.mem
  io.mem.req.valid := false.B
  io.busy := cmd.valid || frontend.io.busy //|| sgunit.io.busy
  io.mem.invalidate_lr := false.B
  io.interrupt := false.B

  io.resp.valid := cmd.valid && is_cr_read
  io.resp.bits.rd := inst.rd
  io.resp.bits.data := crfile.io.rdata

  cmd.ready := (is_transfer && clientArb.io.in(0).req.ready) ||
               //(is_sg && sgunit.io.cpu.req.ready) ||
               is_cr_write || // Write can always go through immediately
               (is_cr_read && io.resp.ready)
}

class CopyAccelerator(implicit p: Parameters) extends LazyRoCC {
  val backend = LazyModule(new DmaBackend)

  tlNode :=* backend.node

  lazy val module = new LazyRoCCModule(this) {
    implicit val edge = tlNode.edgesOut(0)
    val ctrl = Module(new DmaController)

    ctrl.io.cmd <> io.cmd
    io.resp <> ctrl.io.resp
    io.ptw.head <> ctrl.io.ptw
    io.busy := ctrl.io.busy

    backend.module.io.dma <> ctrl.io.dma

    io.mem <> ctrl.io.mem
    io.interrupt := ctrl.io.interrupt
  }
}
