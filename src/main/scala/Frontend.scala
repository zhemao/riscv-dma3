package dma

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.TLEdgeOut
import freechips.rocketchip.util._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile.{HasCoreParameters, CoreBundle, CoreModule}
import freechips.rocketchip.config.Parameters
import DmaRequest._

trait HasClientDmaParameters extends HasCoreParameters with HasDmaParameters {
  val dmaAddrBits = coreMaxAddrBits
  val dmaSegmentSizeBits = coreMaxAddrBits
  val dmaSegmentBits = 24
  val dmaClientIdBits = 2
}

abstract class ClientDmaBundle(implicit val p: Parameters)
  extends ParameterizedBundle()(p) with HasClientDmaParameters
abstract class ClientDmaModule(implicit val p: Parameters)
  extends Module with HasClientDmaParameters

class ClientDmaRequest(implicit p: Parameters) extends ClientDmaBundle()(p) {
  val client_id = UInt(dmaClientIdBits.W)
  val cmd = UInt(DMA_CMD_SZ.W)
  val src_start  = UInt(dmaAddrBits.W)
  val dst_start  = UInt(dmaAddrBits.W)
  val src_stride = UInt(dmaSegmentSizeBits.W)
  val dst_stride = UInt(dmaSegmentSizeBits.W)
  val segment_size = UInt(dmaSegmentSizeBits.W)
  val nsegments  = UInt(dmaSegmentBits.W)
  val alloc = UInt(2.W)

  def isPrefetch(dummy: Int = 0): Bool =
    cmd === DmaRequest.DMA_CMD_PFR || cmd === DmaRequest.DMA_CMD_PFW
}

object ClientDmaRequest {
  val DMA_CMD_RESUME = "b01".U

  def apply(client_id: UInt,
            cmd: UInt,
            src_start: UInt,
            dst_start: UInt,
            segment_size: UInt,
            nsegments: UInt = 1.U,
            src_stride: UInt = 0.U,
            dst_stride: UInt = 0.U,
            alloc: UInt = "b10".U)
      (implicit p: Parameters) = {
    val req = Wire(new ClientDmaRequest)
    req.client_id := client_id
    req.cmd := cmd
    req.src_start := src_start
    req.dst_start := dst_start
    req.src_stride := src_stride
    req.dst_stride := dst_stride
    req.segment_size := segment_size
    req.nsegments := nsegments
    req.alloc := alloc
    req
  }
}
import ClientDmaRequest._

object ClientDmaResponse {
  val NO_ERROR = "b000".U
  val PAUSED = "b001".U
  val SRC_PAGE_FAULT = "b010".U
  val DST_PAGE_FAULT = "b011".U
  val SRC_INVALID_REGION = "b100".U
  val DST_INVALID_REGION = "b101".U

  def apply(client_id: UInt, status: UInt = 0.U, fault_vpn: UInt = 0.U)
           (implicit p: Parameters) = {
    val resp = Wire(new ClientDmaResponse)
    resp.client_id := client_id
    resp.status := status
    resp.fault_vpn := fault_vpn
    resp
  }
}
import ClientDmaResponse._

class ClientDmaResponse(implicit p: Parameters)
    extends ClientDmaBundle()(p) with HasCoreParameters {
  val client_id = UInt(dmaClientIdBits.W)
  val status = UInt(dmaStatusBits.W)
  val fault_vpn = UInt(vpnBits.W)
}

class ClientDmaIO(implicit p: Parameters) extends ParameterizedBundle()(p) {
  val req = Decoupled(new ClientDmaRequest)
  val resp = Flipped(Valid(new ClientDmaResponse))
}

class ClientDmaArbiter(n: Int)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Vec(n, new ClientDmaIO))
    val out = new ClientDmaIO
  })

  val req_arb = Module(new RRArbiter(new ClientDmaRequest, n))
  req_arb.io.in <> io.in.map(_.req)
  io.out.req <> req_arb.io.out

  io.in.zipWithIndex.foreach { case (in, i) =>
    val me = io.out.resp.bits.client_id === i.U
    in.resp.valid := me && io.out.resp.valid
    in.resp.bits := io.out.resp.bits
  }
}

class DecoupledTLB(entries: Int)(implicit edge: TLEdgeOut, p: Parameters) extends CoreModule {
  val lgMaxSize = log2Ceil(coreDataBytes)
  val io = new Bundle {
    val req = Flipped(Decoupled(new TLBReq(lgMaxSize)))
    val resp = Decoupled(new TLBResp)
    val ptw = new TLBPTWIO
  }

  val req = Reg(new TLBReq(lgMaxSize))
  val resp = Reg(new TLBResp)
  val tlb = Module(new TLB(lgMaxSize, entries))

  val s_idle :: s_tlb_req :: s_tlb_resp :: s_done :: Nil = Enum(Bits(), 4)
  val state = Reg(init = s_idle)

  when (io.req.fire()) {
    req := io.req.bits
    state := s_tlb_req
  }

  when (tlb.io.req.fire()) {
    state := s_tlb_resp
  }

  when (state === s_tlb_resp) {
    when (tlb.io.resp.miss) {
      state := s_tlb_req
    } .otherwise {
      resp := tlb.io.resp
      state := s_done
    }
  }

  when (io.resp.fire()) { state := s_idle }

  io.req.ready := state === s_idle

  tlb.io.req.valid := state === s_tlb_req
  tlb.io.req.bits := req

  io.resp.valid := state === s_done
  io.resp.bits := resp

  io.ptw <> tlb.io.ptw
}

class FrontendTLBIO(implicit p: Parameters) extends CoreBundle {
  val lgMaxSize = log2Ceil(coreDataBytes)
  val req = Decoupled(new TLBReq(lgMaxSize))
  val resp = Flipped(Decoupled(new TLBResp))
}

class FrontendTLB(nClients: Int)
    (implicit edge: TLEdgeOut, p: Parameters) extends CoreModule {
  val io = IO(new Bundle {
    val clients = Flipped(Vec(nClients, new FrontendTLBIO))
    val ptw = new TLBPTWIO
  })

  val lgMaxSize = log2Ceil(coreDataBytes)
  val tlbArb = Module(new InOrderArbiter(
    new TLBReq(lgMaxSize), new TLBResp, nClients))
  val tlb = Module(new DecoupledTLB(4))
  tlb.io.req <> tlbArb.io.out_req
  tlbArb.io.out_resp <> tlb.io.resp
  io.ptw <> tlb.io.ptw

  tlbArb.io.in_req <> io.clients.map(_.req)
  io.clients.zip(tlbArb.io.in_resp).foreach {
    case (client, arb_resp) => client.resp <> arb_resp
  }
}

class DmaFrontend(implicit p: Parameters) extends CoreModule()(p)
    with HasClientDmaParameters {
  val io = IO(new Bundle {
    val cpu = Flipped(new ClientDmaIO)
    val dma = new DmaIO
    val tlb = new FrontendTLBIO
    val busy = Output(Bool())
    val pause = Input(Bool())
  })

  private val pgSize = 1 << pgIdxBits

  val cmd = Reg(UInt(DMA_CMD_SZ.W))
  val adv_ptr = MuxLookup(cmd, "b11".U, Seq(
    DMA_CMD_PFR -> "b10".U,
    DMA_CMD_PFW -> "b10".U))
  val client_id = Reg(UInt(dmaClientIdBits.W))

  val segment_size = Reg(UInt(dmaSegmentSizeBits.W))
  val bytes_left = Reg(UInt(dmaSegmentSizeBits.W))
  val segments_left = Reg(UInt(dmaSegmentBits.W))

  val src_vaddr = Reg(UInt(dmaAddrBits.W))
  val dst_vaddr = Reg(UInt(dmaAddrBits.W))
  val src_vpn = src_vaddr(dmaAddrBits - 1, pgIdxBits)
  val dst_vpn = dst_vaddr(dmaAddrBits - 1, pgIdxBits)
  val src_idx = src_vaddr(pgIdxBits - 1, 0)
  val dst_idx = dst_vaddr(pgIdxBits - 1, 0)
  val src_pglen = pgSize.U - src_idx
  val dst_pglen = pgSize.U - dst_idx

  val src_stride = Reg(UInt(dmaSegmentSizeBits.W))
  val dst_stride = Reg(UInt(dmaSegmentSizeBits.W))

  val src_ppn = Reg(UInt(ppnBits.W))
  val dst_ppn = Reg(UInt(ppnBits.W))

  val src_paddr = Cat(src_ppn, src_idx)
  val dst_paddr = Cat(dst_ppn, dst_idx)

  val last_src_vpn = Reg(UInt(vpnBits.W))
  val last_dst_vpn = Reg(UInt(vpnBits.W))

  val tx_len = src_pglen min dst_pglen min bytes_left

  val dma_busy = RegInit(0.U(nDmaXacts.W))
  val dma_xact_id = PriorityEncoder(~dma_busy)

  val alloc = Reg(UInt(2.W))

  val (s_idle :: s_translate :: s_dma_req :: s_dma_update ::
       s_prepare :: s_finish :: Nil) = Enum(6)
  val state = Reg(init = s_idle)

  // lower bit is for src, higher bit is for dst
  val to_translate = Reg(UInt(2.W), init = 0.U)
  val tlb_sent = Reg(UInt(2.W), init = ~0.U(2.W))
  val tlb_to_send = to_translate & ~tlb_sent
  val resp_status = Reg(UInt(dmaStatusBits.W))
  val fault_vpn = Reg(UInt(vpnBits.W))
  val ptw_errors = RegInit(0.U(2.W))
  val send_vpn = PriorityMux(tlb_to_send, Seq(src_vpn, dst_vpn))

  io.tlb.req.valid := tlb_to_send.orR
  io.tlb.req.bits.vaddr := Cat(send_vpn, 0.U(pgIdxBits.W))
  io.tlb.req.bits.passthrough := false.B
  io.tlb.req.bits.instruction := false.B
  io.tlb.req.bits.sfence.valid := PriorityMux(tlb_to_send, ptw_errors.toBools)
  io.tlb.req.bits.sfence.bits.rs1 := true.B
  io.tlb.req.bits.sfence.bits.rs2 := false.B
  io.tlb.req.bits.sfence.bits.addr := Cat(send_vpn, 0.U(pgIdxBits.W))
  io.tlb.resp.ready := tlb_sent.orR

  when (io.tlb.req.fire()) {
    tlb_sent := tlb_sent | PriorityEncoderOH(tlb_to_send)
  }

  when (io.tlb.resp.fire()) {
    val recv_choice_oh = PriorityEncoderOH(to_translate)
    val recv_choice = OHToUInt(recv_choice_oh)(0)
    val page_fault = Mux(recv_choice,
      io.tlb.resp.bits.pf.st, io.tlb.resp.bits.pf.ld)
    val bad_region = Mux(recv_choice,
      alloc(1) && !io.tlb.resp.bits.cacheable,
      alloc(0) && !io.tlb.resp.bits.cacheable)

    when (page_fault || bad_region) {
      resp_status := Mux(page_fault,
        Mux(recv_choice, DST_PAGE_FAULT, SRC_PAGE_FAULT),
        Mux(recv_choice, DST_INVALID_REGION, SRC_INVALID_REGION))
      fault_vpn := Mux(recv_choice, dst_vpn, src_vpn)
      ptw_errors := ptw_errors | recv_choice_oh
    } .otherwise {
      // getting the src translation
      when (recv_choice) {
        dst_ppn := io.tlb.resp.bits.paddr >> pgIdxBits.U
      } .otherwise {
        src_ppn := io.tlb.resp.bits.paddr >> pgIdxBits.U
      }
      ptw_errors := ptw_errors & ~recv_choice_oh
    }
    to_translate := to_translate & ~recv_choice_oh
  }

  io.cpu.req.ready := state === s_idle
  io.cpu.resp.valid := state === s_finish
  io.cpu.resp.bits := ClientDmaResponse(client_id, resp_status, fault_vpn)

  io.dma.req.valid := (state === s_dma_req) && !dma_busy.andR
  io.dma.req.bits := DmaRequest(
    xact_id = dma_xact_id,
    cmd = cmd,
    source = src_paddr,
    dest = dst_paddr,
    length = tx_len,
    alloc = alloc)
  io.dma.resp.ready := true.B

  when (io.cpu.req.fire()) {
    val req = io.cpu.req.bits
    client_id := req.client_id
    when (req.cmd =/= DMA_CMD_RESUME) {
      cmd := req.cmd
      src_vaddr := req.src_start
      dst_vaddr := req.dst_start
      src_stride := req.src_stride
      dst_stride := req.dst_stride
      segment_size := req.segment_size
      segments_left := req.nsegments - 1.U
      bytes_left := req.segment_size
      to_translate := Mux(req.isPrefetch(), "b10".U, "b11".U)
      alloc := req.alloc
    } .otherwise {
      // On resume, retranslate any pages that had errors
      to_translate := ptw_errors
    }
    when (io.pause) {
      resp_status := PAUSED
      state := s_finish
    } .otherwise {
      tlb_sent := 0.U
      state := s_translate
    }
  }

  when (state === s_translate && !to_translate.orR) {
    last_src_vpn := src_vpn
    last_dst_vpn := dst_vpn
    state := Mux(ptw_errors.orR, s_finish, s_dma_req)
  }

  def setBusy(set: Bool, xact_id: UInt): UInt =
    Mux(set, UIntToOH(xact_id), 0.U)

  dma_busy := (dma_busy |
                setBusy(io.dma.req.fire(), dma_xact_id)) &
                ~setBusy(io.dma.resp.fire(), io.dma.resp.bits.xact_id)


  when (io.dma.req.fire()) {
    src_vaddr := src_vaddr + Mux(adv_ptr(0), tx_len, 0.U)
    dst_vaddr := dst_vaddr + Mux(adv_ptr(1), tx_len, 0.U)
    bytes_left := bytes_left - tx_len
    state := s_dma_update
  }

  when (state === s_dma_update) {
    when (bytes_left === 0.U) {
      when (segments_left === 0.U) {
        resp_status := NO_ERROR
        state := s_finish
      } .otherwise {
        src_vaddr := src_vaddr + Mux(adv_ptr(0), src_stride, 0.U)
        dst_vaddr := dst_vaddr + Mux(adv_ptr(1), dst_stride, 0.U)
        bytes_left := segment_size
        segments_left := segments_left - 1.U
        state := s_prepare
      }
    } .otherwise { state := s_prepare }
  }

  when (state === s_prepare) {
    to_translate := adv_ptr & Cat(
      dst_vpn =/= last_dst_vpn,
      src_vpn =/= last_src_vpn)
    when (io.pause) {
      resp_status := PAUSED
      state := s_finish
    } .otherwise {
      tlb_sent := 0.U
      state := s_translate
    }
  }

  when (state === s_finish) { state := s_idle }

  io.busy := (state =/= s_idle) || dma_busy.orR
}
