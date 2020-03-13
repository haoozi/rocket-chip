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


  val cpu_mem_pc       = Input(UInt(width = 64))

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

  io.prefetch_req.valid       := false.B
  io.prefetch_req.bits.addr   := UInt(0)
  io.prefetch_req.bits.cmd    := M_PFW
  io.prefetch_req.bits.data   := UInt(0)
}


/**
  * Next line prefetcher. Grabs the next line
  */

class NLPrefetcher(implicit p: Parameters) extends NBDCachePrefetcher()(p) {
  override lazy val module = new NLPrefetcherImpl(this)
}

class NLPrefetcherImpl(outer: NLPrefetcher) extends NBDCachePrefetcherImpl(outer) {

    val nl_req_valid = RegInit(false.B)
    val nl_req_addr = Reg(UInt(width = coreMaxAddrBits))

    var addr_valid = Bool()
    addr_valid = io.cpu_req_addr > 0x010
    // addr_valid = io.cpu_req_addr > 0x010 && io.cpu_mem_pc > UInt(0x80000000L, width = 64)

    when (io.cpu_req_valid && addr_valid) {
      nl_req_valid := true.B
      nl_req_addr  := io.cpu_req_addr + cacheBlockBytes.U
    } .otherwise {
      nl_req_valid := false.B
    }

    // Handshake
    val req_valid = RegInit(false.B)
    val req_addr = RegEnable(nl_req_addr, nl_req_valid)

    when (io.prefetch_req.fire() && !nl_req_valid) {
      req_valid := false.B
    } .otherwise {
      when (nl_req_valid) {
        req_valid := true.B
      }
    }

    io.prefetch_req.valid            := req_valid
    io.prefetch_req.bits.addr        := req_addr
    io.prefetch_req.bits.cmd         := M_PFW
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


  when (io.cpu_req_valid) {
    req_stride := io.cpu_req_addr - req_addr_history
    req_addr_history := io.cpu_req_addr
  }


  val stride_req_valid = RegInit(false.B)
  val stride_req_addr = Reg(UInt(width = coreMaxAddrBits))

  when (io.cpu_req_valid) {
    when (req_stride < UInt(0x1000)) {
      stride_req_valid := true.B
      stride_req_addr  := io.cpu_req_addr + req_stride
    }.otherwise {
      stride_req_valid := false.B
    }
  } .otherwise {
    stride_req_valid := false.B
  }

  // Handshake
  val req_valid = RegInit(false.B)
  val req_addr = RegEnable(stride_req_addr, stride_req_valid)

  when (io.prefetch_req.fire() && !rpts.io.prefetch_req_valid) {
    req_valid := false.B
  } .otherwise {
    when (rpts.io.prefetch_req_valid) {
      req_valid := true.B
    }
  }


  io.prefetch_req.valid            := req_valid
  io.prefetch_req.bits.addr        := req_addr
  io.prefetch_req.bits.cmd         := M_PFW
  io.prefetch_req.bits.data        := UInt(0)

}
