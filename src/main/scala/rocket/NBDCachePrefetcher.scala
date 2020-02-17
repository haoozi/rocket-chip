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

  val req_valid = RegInit(false.B)
  val req_addr  = Reg(UInt(width = coreMaxAddrBits))
  val req_cmd   = Reg(UInt(M_SZ.W))

  when (io.cpu_req_valid) {
    req_valid := true.B
    req_addr  := io.cpu_req_addr + cacheBlockBytes.U
    req_cmd   := M_PFW
  } .elsewhen (io.prefetch_req.fire()) {
    req_valid := false.B
  }

  io.prefetch_req.valid            := req_valid
  io.prefetch_req.bits.addr        := req_addr
  io.prefetch_req.bits.cmd         := req_cmd
  io.prefetch_req.bits.data        := UInt(0)

}