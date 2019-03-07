package dma

import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import freechips.rocketchip.config.Parameters

import DmaRequest._
import ClientDmaRequest._
import ClientDmaResponse._

class ScatterGatherRequest(implicit p: Parameters) extends ClientDmaBundle()(p) {
  val cmd = UInt(DMA_CMD_SZ.W)
  val src_start = UInt(dmaAddrBits.W)
  val dst_start = UInt(dmaAddrBits.W)
  val segment_size = UInt(dmaSegmentSizeBits.W)
  val nsegments  = UInt(dmaSegmentBits.W)
  val alloc = UInt(2.W)
}

object ScatterGatherRequest {
  val DMA_SG_GATHER = "b00".U
  val DMA_SG_SCATTER = "b01".U
  val DMA_SG_RESUME = "b10".U

  def apply(cmd: UInt,
            src_start: UInt,
            dst_start: UInt,
            segment_size: UInt,
            nsegments: UInt = 0.U,
            alloc: UInt = "b10".U)
      (implicit p: Parameters): ScatterGatherRequest = {
    val req = Wire(new ScatterGatherRequest)
    req.cmd := cmd
    req.src_start := src_start
    req.dst_start := dst_start
    req.segment_size := segment_size
    req.nsegments := nsegments
    req.alloc := alloc
    req
  }
}
import ScatterGatherRequest._

class ScatterGatherResponse(implicit p: Parameters)
    extends ClientDmaBundle()(p) with HasCoreParameters {
  val status = UInt(dmaStatusBits.W)
  val fault_vpn = UInt(vpnBits.W)
}

object ScatterGatherResponse {
  def apply(status: UInt = 0.U, fault_vpn: UInt = 0.U)
           (implicit p: Parameters): ScatterGatherResponse = {
    val resp = Wire(new ScatterGatherResponse)
    resp.status := status
    resp.fault_vpn := fault_vpn
    resp
  }
}

class ScatterGatherIO(implicit val p: Parameters) extends Bundle {
  val req = Decoupled(new ScatterGatherRequest)
  val resp = Flipped(Valid(new ScatterGatherResponse))
}

class ScatterGatherUnit(client_id: Int)(implicit p: Parameters)
    extends CoreModule()(p) with HasClientDmaParameters {
  val io = IO(new Bundle {
    val cpu = Flipped(new ScatterGatherIO)
    val dma = new ClientDmaIO
    val tlb = new FrontendTLBIO
    val mem = new HellaCacheIO
    val busy = Output(Bool())
  })

  require(p(XLen) == 64)

  private val pgSize = 1 << pgIdxBits

  val (s_idle :: s_translate_req :: s_translate_resp :: s_busy ::
       s_wait :: s_finish :: Nil) = Enum(6)
  val state = RegInit(s_idle)

  val tab_vaddr = Reg(UInt(dmaAddrBits.W))
  val tab_vpn = tab_vaddr(dmaAddrBits - 1, pgIdxBits)
  val tab_idx = tab_vaddr(pgIdxBits - 1, 0)
  val tab_ppn = Reg(UInt(ppnBits.W))
  val tab_paddr = Cat(tab_ppn, tab_idx)

  val src_vaddr = Reg(UInt(dmaAddrBits.W))
  val dst_vaddr = Reg(UInt(dmaAddrBits.W))
  val segments_left = Reg(UInt(dmaSegmentBits.W))
  val segment_size = Reg(UInt(dmaAddrBits.W))
  val alloc = Reg(UInt(2.W))
  val scatter = Reg(Bool())
  val resp_status = Reg(UInt(dmaStatusBits.W))
  val fault_vpn = Reg(UInt(vpnBits.W))

  when (io.cpu.req.fire()) {
    val req = io.cpu.req.bits
    when (req.cmd =/= DMA_SG_RESUME) {
      when (req.cmd === DMA_SG_GATHER) {
        tab_vaddr := req.src_start
        scatter := false.B
      } .otherwise {
        tab_vaddr := req.dst_start
        scatter := true.B
      }
      resp_status := NO_ERROR
      src_vaddr := req.src_start
      dst_vaddr := req.dst_start
      segments_left := req.nsegments
      segment_size := req.segment_size
      alloc := req.alloc
      state := s_translate_req
    } .otherwise {
      state := Mux(tab_idx === 0.U, s_translate_req, s_busy)
    }
  }

  when (io.tlb.req.fire()) { state := s_translate_resp }
  when (io.tlb.resp.fire()) {
    when (io.tlb.resp.bits.pf.ld) {
      resp_status := Mux(scatter, DST_PAGE_FAULT, SRC_PAGE_FAULT)
      fault_vpn := tab_vpn
      state := s_finish
    } .otherwise {
      tab_ppn := io.tlb.resp.bits.paddr >> pgIdxBits.U
      state := s_busy
    }
  }

  io.tlb.req.valid := (state === s_translate_req)
  io.tlb.req.bits.vaddr := tab_vpn << pgIdxBits.U
  io.tlb.req.bits.passthrough := false.B
  io.tlb.resp.ready := (state === s_translate_resp)

  val roq_entries = 16 // Parameterize?
  val dma_roq = Reg(Vec(roq_entries, new ClientDmaRequest))
  val dma_roq_valid = RegInit(0.U(roq_entries.W))
  val dma_roq_head = RegInit(0.U(log2Ceil(roq_entries).W))
  val dma_roq_tail = RegInit(0.U(log2Ceil(roq_entries).W))
  val maybe_full = RegInit(false.B)
  val dma_roq_empty = !maybe_full && dma_roq_head === dma_roq_tail
  val dma_roq_full = maybe_full && dma_roq_head === dma_roq_tail

  val dma_roq_deq = io.dma.resp.valid && io.dma.resp.bits.status === NO_ERROR
  val dma_roq_enq = io.mem.req.fire()

  when (dma_roq_deq =/= dma_roq_enq) {
    maybe_full := dma_roq_enq
  }

  io.mem.req.valid := (state === s_busy) && !dma_roq_full
  io.mem.req.bits.tag := dma_roq_head
  io.mem.req.bits.addr := tab_paddr
  io.mem.req.bits.cmd := M_XRD
  io.mem.req.bits.typ := MT_D
  io.mem.req.bits.phys := true.B

  when (io.mem.req.fire()) {
    dma_roq(dma_roq_head) := ClientDmaRequest(
      client_id = client_id.U,
      cmd = DMA_CMD_COPY,
      src_start = src_vaddr,
      dst_start = dst_vaddr,
      segment_size = segment_size)
    dma_roq_head := dma_roq_head + 1.U
    src_vaddr := src_vaddr + segment_size
    dst_vaddr := dst_vaddr + segment_size
    tab_vaddr := tab_vaddr + 8.U
    segments_left := segments_left - 1.U
    when (segments_left === 1.U || tab_idx === (pgSize - 8).U) {
      state := s_wait
    }
  }

  when (io.mem.resp.valid) {
    val tag = io.mem.resp.bits.tag
    val data = io.mem.resp.bits.data
    when (scatter) {
      dma_roq(tag).dst_start := data
    } .otherwise {
      dma_roq(tag).src_start := data
    }
  }

  dma_roq_valid := (dma_roq_valid &
    ~Mux(dma_roq_deq, UIntToOH(dma_roq_tail), 0.U)) |
    Mux(io.mem.resp.valid, UIntToOH(io.mem.resp.bits.tag), 0.U)

  val dma_sending = RegInit(true.B)
  val dma_can_send = dma_sending && (dma_roq_valid >> dma_roq_tail)(0)

  io.dma.req.valid := state.isOneOf(s_busy, s_wait) && dma_can_send
  io.dma.req.bits := dma_roq(dma_roq_tail)

  when (io.dma.req.fire()) {
    dma_sending := false.B
  }

  when (io.dma.resp.valid) {
    when (io.dma.resp.bits.status =/= NO_ERROR) {
      resp_status := io.dma.resp.bits.status
      fault_vpn := io.dma.resp.bits.fault_vpn
      state := s_finish
    } .otherwise {
      dma_roq_tail := dma_roq_tail + 1.U
    }
    dma_sending := true.B
  }

  when (state === s_wait && dma_roq_empty) {
    when (segments_left === 0.U) {
      resp_status := NO_ERROR
      state := s_finish
    } .otherwise {
      state := s_translate_req
    }
  }

  when (state === s_finish) {
    state := s_idle
  }

  io.cpu.req.ready := state === s_idle
  io.cpu.resp.valid := state === s_finish
  io.cpu.resp.bits := ScatterGatherResponse(resp_status, fault_vpn)
  io.busy := state =/= s_idle
}
