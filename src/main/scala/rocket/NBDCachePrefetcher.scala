// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket

import Chisel._
import Chisel.ImplicitConversions._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._


class NBDCachePrefetcherBundle()(implicit p: Parameters) extends CoreBundle with HasCoreParameters {

  val cpu_req_valid    = Input(Bool())
  val cpu_req_addr     = Input(UInt(width = coreMaxAddrBits))
  val cpu_req_cmd      = Input(Bits(M_SZ.W))
  val cpu_req_size     = Input(Bits(width = log2Ceil(coreDataBytes.log2 + 1)))

  // val cache_hit        = Input(Bool())

  val prefetch_req     = Decoupled(new HellaCacheReq)
}





abstract class NBDCachePrefetcher(implicit p: Parameters) extends LazyModule
{
  private val tileParams = p(TileKey)
  protected val cfg = tileParams.dcache.get

  val module: NBDCachePrefetcherImpl

}

class NBDCachePrefetcherImpl(outer: NBDCachePrefetcher) extends LazyModuleImp(outer)
  with HasL1HellaCacheParameters{
  val io = IO(new NBDCachePrefetcherBundle())
}




/**
  * DummpyPrefetcher: do nothing
  */

class DummyPrefetcher(implicit p: Parameters) extends NBDCachePrefetcher()(p) {
  override lazy val module = new DummyPrefetcherImpl(this)
}

class DummyPrefetcherImpl(outer: DummyPrefetcher) extends NBDCachePrefetcherImpl(outer) {

  io.prefetch_req.valid := false.B
}


/**
  * Next line prefetcher. Grabs the next line
  */

class NLPrefetcher(implicit p: Parameters) extends NBDCachePrefetcher()(p) {
  override lazy val module = new NLPrefetcherImpl(this)
}

class NLPrefetcherImpl(outer: NLPrefetcher) extends NBDCachePrefetcherImpl(outer) {

  // val degree = 2
  //
  // val req_queue = Module(new Queue(UInt(width = coreMaxAddrBits), 4))
  //
  // val counterEn = RegInit(Bool(false))
  // val counter = new Counter(degree + 1)
  // val counterWrap = RegInit(Bool(false))
  //
  // when (counterEn) {
  //   counterWrap := counter.inc()
  // }
  //
  // when (counterEn && !counterWrap) {
  //   req_queue.io.enq.bits := io.cpu_req_addr + (counter.value + 1) * cacheBlockBytes.U
  //   req_queue.io.enq.valid := Bool(true)
  // } .otherwise {
  //   req_queue.io.enq.valid := Bool(false)
  // }
  //
  // when (counterWrap) {
  //   counterEn := Bool(false)
  // }
  //
  // when (io.cpu_req_valid) {
  //   counterEn := Bool(true)
  // }
  //
  // // io.prefetch_req.valid            := RegNext(req_queue.io.deq.valid) && !RegNext(io.cpu_req_valid)
  // io.prefetch_req.valid            := ShiftRegister(req_queue.io.deq.valid, 10, Bool(false), en = Bool(true))
  // req_queue.io.deq.ready           := io.prefetch_req.ready
  //
  // io.prefetch_req.bits.addr        := ShiftRegister(req_queue.io.deq.bits, 10)
  // io.prefetch_req.bits.cmd         := M_PFW
  // io.prefetch_req.bits.data        := UInt(0)

  val req_valid = RegInit(false.B)
  val req_addr  = Reg(UInt(width = coreMaxAddrBits))
  val req_cmd   = Reg(UInt(M_SZ.W))

  when (io.cpu_req_valid) {
    req_valid := true.B
    req_addr  := io.cpu_req_addr + cacheBlockBytes.U
    req_cmd   := M_PFW
  } .otherwise {
    req_valid := false.B
  }

  io.prefetch_req.valid            := ShiftRegister(req_valid, 5, Bool(false), en = true)
  io.prefetch_req.bits.addr        := ShiftRegister(req_addr, 5)
  io.prefetch_req.bits.cmd         := ShiftRegister(req_cmd, 5)
  io.prefetch_req.bits.data        := UInt(0)

}



/**
  * Stride prefetcher.
  */


class StridePrefetcher(implicit p: Parameters) extends NBDCachePrefetcher()(p) {
  override lazy val module = new StridePrefetcherImpl(this)
}

class StridePrefetcherImpl(outer: StridePrefetcher) extends NBDCachePrefetcherImpl(outer) {
  // val degree = 3

  val req_addr_history = Reg(UInt(width = coreMaxAddrBits))
  val req_stride = Reg(UInt(width = coreMaxAddrBits))

  // val req_queue = Module(new Queue(UInt(width = coreMaxAddrBits), 4))
  //
  //
  // val counterEn = RegInit(Bool(false))
  // val counter = new Counter(degree + 1)
  // val counterWrap = RegInit(Bool(false))
  //
  // when (counterEn) {
  //   counterWrap := counter.inc()
  // }
  //
  // when (counterEn && !counterWrap) {
  //   when (req_stride < UInt(0x1000)) {
  //     req_queue.io.enq.bits := io.cpu_req_addr + (counter.value + 1) * req_stride
  //     req_queue.io.enq.valid := Bool(true)
  //   } .otherwise {
  //     req_queue.io.enq.valid := Bool(false)
  //   }
  // } .otherwise {
  //   req_queue.io.enq.valid := Bool(false)
  // }
  //
  //
  // when (counterWrap) {
  //   counterEn := Bool(false)
  // }
  //
  when (io.cpu_req_valid) {
    req_stride := io.cpu_req_addr - req_addr_history
    req_addr_history := io.cpu_req_addr
  //
  //   counterEn := Bool(true)
  }
  //
  //
  // io.prefetch_req.valid         := req_queue.io.deq.valid
  // req_queue.io.deq.ready        := io.prefetch_req.ready
  //
  // io.prefetch_req.bits.addr     := req_queue.io.deq.bits
  // io.prefetch_req.bits.cmd      := M_PFW
  // io.prefetch_req.bits.data     := UInt(0)

  val req_valid = RegInit(false.B)
  val req_addr  = Reg(UInt(width = coreMaxAddrBits))
  val req_cmd   = Reg(UInt(M_SZ.W))

  when (io.cpu_req_valid) {
    when (req_stride < UInt(0x1000)) {
      req_valid := true.B
      req_addr  := io.cpu_req_addr + req_stride
      req_cmd   := M_PFW
    }.otherwise {
      req_valid := false.B
    }
  } .otherwise {
    req_valid := false.B
  }

  io.prefetch_req.valid            := ShiftRegister(req_valid, 5, Bool(false), en = true)
  io.prefetch_req.bits.addr        := ShiftRegister(req_addr, 5)
  io.prefetch_req.bits.cmd         := ShiftRegister(req_cmd, 5)
  io.prefetch_req.bits.data        := UInt(0)


}
