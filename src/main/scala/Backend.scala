package dma

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.coreplex.CacheBlockBytes
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, IdRange}
import freechips.rocketchip.rocket.PAddrBits
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

case class DmaConfig(
  nTrackers: Int = 1,
  nDmaXacts: Int = 4,
  nMemXacts: Int = 4,
  pipelineDepth: Int = 16)

case object DmaKey extends Field[DmaConfig]

trait HasDmaParameters {
  implicit val p: Parameters
  val dmaExternal = p(DmaKey)
  val nDmaTrackers = dmaExternal.nTrackers
  val nDmaXacts = dmaExternal.nDmaXacts
  val nDmaTrackerMemXacts = dmaExternal.nMemXacts
  val dmaXactIdBits = log2Up(nDmaXacts)
  val pipelineDepth = dmaExternal.pipelineDepth
  val pipelineIdxBits = log2Up(pipelineDepth)
  val pipelineCountBits = log2Up(pipelineDepth+1)
  val dmaStatusBits = 3
  val addrBits = p(PAddrBits)
  val blockBytes = p(CacheBlockBytes)
  val blockOffset = log2Ceil(blockBytes)
  val blockAddrBits = addrBits - blockOffset
}

trait DmaTrackerUtils extends HasDmaParameters {
  val edge: TLEdge

  lazy val dataBits = edge.bundle.dataBits
  lazy val dataBytes = dataBits / 8
  lazy val dataBeats = blockBytes / dataBytes
  lazy val beatAddrBits = log2Ceil(dataBeats)
  lazy val byteAddrBits = log2Ceil(dataBytes)

  def incWrap(cur: UInt, inc: UInt): UInt = {
    val unwrapped = cur +& inc
    Mux(unwrapped >= pipelineDepth.U, unwrapped - pipelineDepth.U, unwrapped)
  }
}

abstract class DmaModule(implicit val p: Parameters) extends Module with HasDmaParameters
abstract class DmaBundle(implicit val p: Parameters) extends ParameterizedBundle()(p) with HasDmaParameters

class DmaRequest(implicit p: Parameters) extends DmaBundle()(p) {
  val xact_id = UInt(dmaXactIdBits.W)
  val cmd = UInt(DmaRequest.DMA_CMD_SZ.W)
  val source = UInt(addrBits.W)
  val dest = UInt(addrBits.W)
  val length = UInt(addrBits.W)
  val alloc = UInt(2.W)

  def isPrefetch(dummy: Int = 0): Bool =
    cmd === DmaRequest.DMA_CMD_PFR || cmd === DmaRequest.DMA_CMD_PFW
}

class DmaResponse(implicit p: Parameters) extends DmaBundle()(p) {
  val xact_id = UInt(dmaXactIdBits.W)
  val status = UInt(dmaStatusBits.W)
}

object DmaRequest {
  val DMA_CMD_SZ = 2

  val DMA_CMD_COPY = "b00".U
  val DMA_CMD_PFR  = "b10".U
  val DMA_CMD_PFW  = "b11".U

  def apply(xact_id: UInt = 0.U,
            cmd: UInt,
            source: UInt,
            dest: UInt,
            length: UInt,
            alloc: UInt = "b10".U)(implicit p: Parameters): DmaRequest = {
    val req = Wire(new DmaRequest)
    req.xact_id := xact_id
    req.cmd := cmd
    req.source := source
    req.dest := dest
    req.length := length
    req.alloc := alloc
    req
  }
}
import DmaRequest._

class DmaIO(implicit p: Parameters) extends DmaBundle()(p) {
  val req = Decoupled(new DmaRequest)
  val resp = Flipped(Decoupled(new DmaResponse))
}

class DmaBackend(implicit p: Parameters) extends LazyModule 
    with HasDmaParameters {
  val trackers = Seq.tabulate(nDmaTrackers) {
    i => LazyModule(new DmaTracker(i))
  }
  val node = TLOutputNode()

  trackers.foreach { node := _.node }

  lazy val module = new DmaBackendModule(this)
}

class DmaBackendModule(outer: DmaBackend) extends LazyModuleImp(outer)
    with HasDmaParameters {
  val io = IO(new Bundle {
    val dma = Flipped(new DmaIO)
    val mem = outer.node.bundleOut
  })

  val trackers = outer.trackers.map(_.module)
  val reqReadys = trackers.map(_.io.dma.req.ready).asUInt

  if (nDmaTrackers > 1) {
    val resp_arb = Module(new RRArbiter(new DmaResponse, nDmaTrackers))
    resp_arb.io.in <> trackers.map(_.io.dma.resp)
    io.dma.resp <> resp_arb.io.out

    val selection = PriorityEncoder(reqReadys)
    trackers.zipWithIndex.foreach { case (tracker, i) =>
      tracker.io.dma.req.valid := io.dma.req.valid && selection === i.U
      tracker.io.dma.req.bits := io.dma.req.bits
    }
    io.dma.req.ready := reqReadys.orR
  } else {
    trackers.head.io.dma <> io.dma
  }
}

class PipelinePacket(dataBits: Int)(implicit p: Parameters) extends DmaBundle {
  val data = UInt(dataBits.W)
  val bytes = UInt(log2Up(dataBits/8).W)

  override def cloneType =
    new PipelinePacket(dataBits)(p).asInstanceOf[this.type]
}

class ReservationRequest extends Bundle {
  val multibeat = Bool()
}

class ReservationResponse(implicit val p: Parameters)
    extends ParameterizedBundle
    with HasDmaParameters {
  val idx = UInt(pipelineIdxBits.W)
}

class ReservationData(dataBits: Int)(implicit p: Parameters)
    extends DmaBundle {
  val data = UInt(dataBits.W)
  val bytes = UInt(log2Ceil(dataBits/8).W)
  val idx = UInt(pipelineIdxBits.W)

  override def cloneType =
    new ReservationData(dataBits)(p).asInstanceOf[this.type]
}

class ReservationInputIO(dataBits: Int)(implicit p: Parameters)
    extends Bundle {
  val req = Decoupled(new ReservationRequest)
  val resp = Flipped(Decoupled(new ReservationResponse))
  val data = Decoupled(new ReservationData(dataBits))

  override def cloneType =
    new ReservationInputIO(dataBits)(p).asInstanceOf[this.type]
}

class ReservationOutputIO(dataBits: Int)(implicit p: Parameters)
    extends DmaBundle {
  val count = Output(UInt(pipelineCountBits.W))
  val data = Decoupled(new PipelinePacket(dataBits))

  override def cloneType =
    new ReservationOutputIO(dataBits)(p).asInstanceOf[this.type]
}

class ReservationQueue(val edge: TLEdge)(implicit val p: Parameters)
    extends Module with DmaTrackerUtils {

  val io = IO(new Bundle {
    val in = Flipped(new ReservationInputIO(dataBits))
    val out = new ReservationOutputIO(dataBits)
  })

  val req = Queue(io.in.req, 1)

  val pkt_buffer = Mem(pipelineDepth, new PipelinePacket(dataBits))
  val pkt_valid = Reg(init = 0.U(pipelineDepth.W))

  val head = Reg(init = 0.U(pipelineIdxBits.W))
  val tail = Reg(init = 0.U(pipelineIdxBits.W))
  val count = Reg(init = 0.U(pipelineCountBits.W))

  val req_count = Mux(req.bits.multibeat, dataBeats.U, 1.U)
  count := count + Mux(req.fire(), req_count, 0.U) - io.out.data.fire()

  val full = (count + req_count) > pipelineDepth.U

  req.ready := io.in.resp.ready && !full
  io.in.resp.valid := req.valid && !full
  io.in.resp.bits.idx := tail

  io.in.data.ready := true.B
  io.out.data.valid := (pkt_valid >> head)(0)
  io.out.data.bits := pkt_buffer(head)
  io.out.count := PopCount(pkt_valid)

  when (req.fire()) {
    tail := incWrap(tail, req_count)
  }

  when (io.in.data.fire()) {
    val pkt = Wire(new PipelinePacket(dataBits))
    pkt.data := io.in.data.bits.data
    pkt.bytes := io.in.data.bits.bytes
    pkt_buffer(io.in.data.bits.idx) := pkt
  }

  when (io.out.data.fire()) {
    head := incWrap(head, 1.U)
  }

  pkt_valid := (pkt_valid &
    ~Mux(io.out.data.fire(), UIntToOH(head), 0.U)) |
    Mux(io.in.data.fire(), UIntToOH(io.in.data.bits.idx), 0.U)
}

class DmaTrackerPrefetcher(id: Int)(implicit p: Parameters)
    extends LazyModule with HasDmaParameters {

  val node = TLClientNode(TLClientParameters(
    name = s"dma-prefetcher${id}",
    sourceId = IdRange(0, nDmaTrackerMemXacts)))

  lazy val module = new DmaTrackerPrefetcherModule(this)
}

class DmaTrackerPrefetcherModule(outer: DmaTrackerPrefetcher)
    extends LazyModuleImp(outer) with HasDmaParameters {
  val io = IO(new Bundle {
    val dma = Flipped(new DmaIO)
    val mem = outer.node.bundleOut
  })

  val tl = io.mem(0)
  val edge = outer.node.edgesOut(0)
  val dst_block = Reg(UInt(blockAddrBits.W))
  val bytes_left = Reg(UInt(addrBits.W))

  val prefetch_type = Reg(UInt(TLHints.width.W))
  val prefetch_busy = Reg(UInt(nDmaTrackerMemXacts.W), init = 0.U)
  val prefetch_id_onehot = PriorityEncoderOH(~prefetch_busy)
  val prefetch_id = OHToUInt(prefetch_id_onehot)

  val dma_req_id = Reg(io.dma.req.bits.xact_id.cloneType)

  prefetch_busy := (prefetch_busy |
    Mux(tl.a.fire(), UIntToOH(prefetch_id), 0.U)) &
    ~Mux(tl.d.fire(), UIntToOH(tl.d.bits.source), 0.U)

  val s_idle :: s_prefetch :: s_resp :: Nil = Enum(3)
  val state = Reg(init = s_idle)

  tl.a.valid := (state === s_prefetch) && !prefetch_busy.andR
  tl.a.bits := edge.Hint(
    fromSource = prefetch_id,
    toAddress = dst_block << blockOffset.U,
    lgSize = blockOffset.U,
    param = prefetch_type)._2
  tl.d.ready := prefetch_busy.orR

  io.dma.req.ready := state === s_idle
  io.dma.resp.valid := (state === s_resp) && !prefetch_busy.orR
  io.dma.resp.bits.xact_id := dma_req_id
  io.dma.resp.bits.status := 0.U

  when (io.dma.req.fire()) {
    val dst_off = io.dma.req.bits.dest(blockOffset - 1, 0)
    dst_block := io.dma.req.bits.dest(addrBits - 1, blockOffset)
    bytes_left := io.dma.req.bits.length + dst_off
    dma_req_id := io.dma.req.bits.xact_id
    prefetch_type := Mux(io.dma.req.bits.cmd === DMA_CMD_PFW,
      TLHints.PREFETCH_WRITE, TLHints.PREFETCH_READ)
    state := s_prefetch
  }

  when (tl.a.fire()) {
    when (bytes_left < blockBytes.U) {
      bytes_left := 0.U
      state := s_resp
    } .otherwise {
      bytes_left := bytes_left - blockBytes.U
      dst_block := dst_block + 1.U
    }
  }

  when (io.dma.resp.fire()) { state := s_idle }
}

class DmaTrackerReader(id: Int)(implicit p: Parameters)
    extends LazyModule with HasDmaParameters {
  val node = TLClientNode(TLClientParameters(
    name = s"dma-reader${id}",
    sourceId = IdRange(0, nDmaTrackerMemXacts)))

  lazy val module = new DmaTrackerReaderModule(this)
}

class DmaTrackerReaderModule(outer: DmaTrackerReader)
    extends LazyModuleImp(outer)
    with DmaTrackerUtils {

  val edge = outer.node.edgesOut(0)
  val io = IO(new Bundle {
    val dma_req = Flipped(Decoupled(new DmaRequest))
    val mem = outer.node.bundleOut
    val res = new ReservationInputIO(dataBits)
  })
  val tl = io.mem(0)

  val src_addr = Reg(UInt(addrBits.W))
  val src_block = src_addr(addrBits - 1, blockOffset)
  val src_beat = src_addr(blockOffset - 1, byteAddrBits)
  val src_byte_off = src_addr(byteAddrBits - 1, 0)
  val bytes_left = Reg(UInt(addrBits.W))

  val s_idle :: s_reserve :: s_mem_req :: Nil = Enum(3)
  val state = Reg(init = s_idle)

  val get_busy = Reg(init = 0.U(nDmaTrackerMemXacts.W))
  val byte_offset = Mem(nDmaTrackerMemXacts, UInt(byteAddrBits.W))
  val bytes_valid = Mem(nDmaTrackerMemXacts, UInt(byteAddrBits.W))
  val get_id_onehot = PriorityEncoderOH(~get_busy)
  val get_id = Reg(UInt(log2Up(nDmaTrackerMemXacts).W))
  val data_index = Reg(Vec(nDmaTrackerMemXacts, UInt(log2Up(pipelineDepth).W)))

  val alloc = Reg(Bool())
  val send_block =
    src_beat === 0.U && src_byte_off === 0.U &&
    bytes_left >= blockBytes.U

  io.dma_req.ready := (state === s_idle)

  when (io.dma_req.fire()) {
    src_addr := io.dma_req.bits.source
    bytes_left := io.dma_req.bits.length
    alloc := io.dma_req.bits.alloc(0)
    state := s_reserve
  }

  when (io.res.req.fire()) {
    get_id := OHToUInt(get_id_onehot)
    state := s_mem_req
  }

  when (io.res.resp.fire()) {
    data_index(get_id) := io.res.resp.bits.idx
  }

  when (tl.a.fire()) {
    val bytes_to_read =
      Mux(send_block, blockBytes.U, dataBytes.U - src_byte_off)

    src_addr := src_addr + bytes_to_read
    byte_offset(get_id) := src_byte_off
    bytes_valid(get_id) := Mux(bytes_to_read > dataBytes.U, 0.U,
                           Mux(bytes_to_read < bytes_left, bytes_to_read, bytes_left))

    when (bytes_left > bytes_to_read) {
      bytes_left := bytes_left - bytes_to_read
      state := s_reserve
    } .otherwise {
      bytes_left := 0.U
      state := s_idle
    }
  }

  io.res.req.valid := (state === s_reserve) && !get_busy.andR
  io.res.req.bits.multibeat := send_block

  io.res.resp.ready := (state === s_mem_req) && tl.a.ready
  tl.a.valid := (state === s_mem_req) && io.res.resp.valid
  tl.a.bits := edge.Get(
    fromSource = get_id,
    toAddress = Cat(src_block,
      Mux(send_block, 0.U(beatAddrBits.W), src_beat),
      0.U(byteAddrBits.W)),
    lgSize = Mux(send_block, log2Ceil(blockBytes).U, byteAddrBits.U))._2

  val grant_id = tl.d.bits.source

  get_busy := (get_busy | Mux(io.res.req.fire(), get_id_onehot, 0.U)) &
                          ~Mux(tl.d.fire() && edge.last(tl.d),
                            UIntToOH(grant_id), 0.U)

  when (io.res.data.fire()) {
    data_index(grant_id) := incWrap(data_index(grant_id), 1.U)
  }

  tl.d.ready := io.res.data.ready
  io.res.data.valid := tl.d.valid
  io.res.data.bits.idx := data_index(grant_id)
  io.res.data.bits.data := tl.d.bits.data >> Cat(byte_offset(grant_id), 0.U(3.W))
  io.res.data.bits.bytes := bytes_valid(grant_id) - 1.U
}

class DmaTrackerWriter(id: Int)(implicit p: Parameters)
    extends LazyModule with HasDmaParameters {
  val node = TLClientNode(TLClientParameters(
    name = s"dma-writer${id}",
    sourceId = IdRange(0, nDmaTrackerMemXacts)))

  lazy val module = new DmaTrackerWriterModule(this)
}

class DmaTrackerWriterModule(outer: DmaTrackerWriter)
    extends LazyModuleImp(outer)
    with DmaTrackerUtils {

  val edge = outer.node.edgesOut(0)
  val io = IO(new Bundle {
    val dma = Flipped(new DmaIO)
    val mem = outer.node.bundleOut
    val pipe = Flipped(new ReservationOutputIO(dataBits))
  })
  val tl = io.mem(0)

  val dst_addr = Reg(UInt(addrBits.W))
  val dst_block = dst_addr(addrBits - 1, blockOffset)
  val dst_beat = dst_addr(blockOffset - 1, byteAddrBits)
  val dst_byte_off = dst_addr(byteAddrBits - 1, 0)
  val bytes_left = Reg(UInt(addrBits.W))

  val dma_req_id = Reg(io.dma.req.bits.xact_id.cloneType)

  val s_idle :: s_mem_req :: s_resp :: Nil = Enum(3)
  val state = Reg(init = s_idle)

  val last_data = Reg(UInt(dataBits.W))
  val last_bytes_val = Reg(UInt((log2Up(dataBytes) + 1).W))

  val put_busy = Reg(UInt(nDmaTrackerMemXacts.W), init = 0.U)
  val put_id_onehot = PriorityEncoderOH(~put_busy)
  val put_id = OHToUInt(put_id_onehot)
  val put_block_id = RegEnable(put_id, tl.a.fire() && edge.first(tl.a))

  val data = last_data | Mux(io.pipe.data.valid,
    (io.pipe.data.bits.data << Cat(last_bytes_val, 0.U(3.W))), 0.U)
  val bytes_val = Mux(io.pipe.data.valid,
    last_bytes_val + io.pipe.data.bits.bytes + 1.U,
    last_bytes_val)

  val off_size = dataBytes.U - dst_byte_off
  val needs_more = (bytes_val < off_size) && (bytes_val < bytes_left)
  val flush_buffer = (last_bytes_val >= bytes_left)

  val bytes_to_send = Mux(bytes_val < off_size, bytes_val, off_size)
  val shift_data = (data << Cat(dst_byte_off, 0.U(3.W)))(dataBits-1, 0)
  val write_mask = (((1.U << bytes_to_send) - 1.U) << dst_byte_off)(dataBytes-1, 0)

  val send_block = Reg(init = false.B)
  val alloc = Reg(Bool())
  val block_acquire = send_block && (io.pipe.count < (dataBeats.U - dst_beat))
  val acquire_ok = (state === s_mem_req) &&
                   (!put_busy.andR || send_block && dst_beat =/= 0.U) &&
                   !block_acquire

  tl.a.valid := acquire_ok && !needs_more && (io.pipe.data.valid || flush_buffer)
  tl.a.bits := edge.Put(
    fromSource = Mux(send_block && dst_beat =/= 0.U, put_block_id, put_id),
    toAddress = Cat(
      dst_block,
      Mux(send_block, 0.U(beatAddrBits.W), dst_beat),
      0.U(byteAddrBits.W)),
    lgSize = Mux(send_block, blockOffset.U, byteAddrBits.U),
    data = shift_data,
    mask = write_mask)._2
  tl.d.ready := put_busy.orR

  io.pipe.data.ready := (acquire_ok && tl.a.ready && !flush_buffer) || needs_more

  put_busy := (put_busy |
    Mux(tl.a.fire() && edge.first(tl.a), UIntToOH(put_id), 0.U)) &
    ~Mux(tl.d.fire(), UIntToOH(tl.d.bits.source), 0.U)

  io.dma.req.ready := (state === s_idle)

  io.dma.resp.valid := (state === s_resp) && !put_busy.orR
  io.dma.resp.bits.xact_id := dma_req_id
  io.dma.resp.bits.status := 0.U

  when (io.dma.req.fire()) {
    dma_req_id := io.dma.req.bits.xact_id
    dst_addr := io.dma.req.bits.dest
    bytes_left := io.dma.req.bits.length
    last_data := 0.U
    last_bytes_val := 0.U
    alloc := io.dma.req.bits.alloc(1)
    send_block := io.dma.req.bits.dest(blockOffset - 1, 0) === 0.U &&
                  io.dma.req.bits.length >= blockBytes.U
    state := s_mem_req
  }

  when (io.pipe.data.fire() && needs_more) {
    last_data := data
    last_bytes_val := bytes_val
  }

  when (tl.a.fire()) {
    val next_addr = dst_addr + bytes_to_send
    val next_bytes_left = bytes_left - bytes_to_send

    last_bytes_val := bytes_val - bytes_to_send
    last_data := data >> Cat(bytes_to_send, 0.U(3.W))
    bytes_left := next_bytes_left
    dst_addr := next_addr

    when (next_bytes_left === 0.U) {
      state := s_resp
    }

    when (next_addr(blockOffset - 1, 0) === 0.U) {
      send_block := next_bytes_left >= blockBytes.U
    }
  }

  when (io.dma.resp.fire()) { state := s_idle }
}

class DmaTracker(id: Int)(implicit p: Parameters) extends LazyModule {
  val prefetch = LazyModule(new DmaTrackerPrefetcher(id))
  val reader = LazyModule(new DmaTrackerReader(id))
  val writer = LazyModule(new DmaTrackerWriter(id))
  val xbar = LazyModule(new TLXbar)
  val node = TLOutputNode()

  xbar.node := prefetch.node
  xbar.node := reader.node
  xbar.node := writer.node
  node := xbar.node

  lazy val module = new DmaTrackerModule(this)
}

class DmaTrackerModule(outer: DmaTracker) extends LazyModuleImp(outer)
    with DmaTrackerUtils {

  val edge = outer.node.edgesOut(0)
  val io = IO(new Bundle {
    val dma = Flipped(new DmaIO)
    val mem = outer.node.bundleOut
  })

  require(pipelineDepth >= 1)
  // we can't have more outstanding requests than we have pipeline space
  require(nDmaTrackerMemXacts <= pipelineDepth)
  // The pipeline must at least be able to hold a full block
  require(pipelineDepth >= dataBeats)

  val prefetch = outer.prefetch.module
  val reader = outer.reader.module
  val writer = outer.writer.module
  val resq = Module(new ReservationQueue(edge))
  resq.io.in <> reader.io.res
  writer.io.pipe <> resq.io.out

  val s_idle :: s_prefetch :: s_read :: s_write :: Nil = Enum(4)
  val state = Reg(init = s_idle)

  val is_prefetch = io.dma.req.bits.isPrefetch()
  val is_copy = io.dma.req.bits.cmd === DMA_CMD_COPY

  val req = Reg(new DmaRequest)

  io.dma.req.ready := (state === s_idle)

  prefetch.io.dma.req.valid := (state === s_prefetch)
  prefetch.io.dma.req.bits := req

  reader.io.dma_req.valid := (state === s_read)
  reader.io.dma_req.bits := req

  writer.io.dma.req.valid := (state === s_write)
  writer.io.dma.req.bits := req

  when (io.dma.req.fire()) {
    req := io.dma.req.bits
    state := MuxCase(s_idle, Seq(
      is_prefetch -> s_prefetch,
      is_copy -> s_read))
  }

  when (prefetch.io.dma.req.fire()) {
    state := s_idle
  }

  when (reader.io.dma_req.fire()) {
    state := s_write
  }

  when (writer.io.dma.req.fire()) {
    state := s_idle
  }

  val respArb = Module(new RRArbiter(new DmaResponse, 2))
  respArb.io.in <> Seq(prefetch.io.dma.resp, writer.io.dma.resp)
  io.dma.resp <> respArb.io.out
}
