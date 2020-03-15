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

  addr_valid = io.cpu_mem_pc > UInt(0x80000000L, width = 64) && io.cpu_req_addr > UInt(0x80000000L, width = 64)

  when (io.cpu_req_valid && addr_valid) {
    nl_req_valid := true.B
    nl_req_addr  := (io.cpu_req_addr + cacheBlockBytes.U) & UInt(0xFFFFFFFFFFFFFC0L)
  } .otherwise {
    nl_req_valid := false.B
  }

  // Handshake
  val req_valid = RegInit(false.B)
  val req_addr = RegEnable(nl_req_addr, nl_req_valid)

  when (nl_req_valid) {
    req_valid := true.B
  } .elsewhen(io.prefetch_req.fire()) {
    req_valid := false.B
  }

  io.prefetch_req.valid            := req_valid
  io.prefetch_req.bits.addr        := req_addr
  io.prefetch_req.bits.cmd         := M_PFW
  io.prefetch_req.bits.data        := UInt(0)

}



/**
  * Stride prefetcher.
  */


class RPTEntry(id: Int)(p: Parameters) extends L1HellaCacheModule()(p) {
  val dropCycles: Int = 1000

  val io = new Bundle {
    val cpu_req_valid  = Input(Bool())
    val cpu_req_addr   = Input(UInt(width = coreMaxAddrBits))

    val cpu_mem_pc     = Input(UInt(width = 64))


    val cpu_req_pc_match = Output(Bool())
    val prefetch_valid  = Output(Bool())
    val prefetch_addr   = Output(UInt(width = coreMaxAddrBits))


    val alloc_ready = Output(Bool())
    val alloc_do = Input(Bool())

    val state_steady   = Output(Bool())
    val state_idle   = Output(Bool())
    val state_trans  = Output(Bool())
  }


  val s_idle :: s_trans :: s_steady :: Nil = Enum(UInt(), 3)
  val state = Reg(init=s_idle)


  val rpte_pc = Reg(UInt(width = 64))
  val rpte_addr = Reg(UInt(width = coreMaxAddrBits))
  val rpte_stride = Reg(UInt())

  val rpte_counter = new Counter(dropCycles)
  val rpte_counter_wrap = RegInit(Bool(false))
  rpte_counter_wrap := rpte_counter.inc()

  val rpte_predict = Reg(UInt(width = coreMaxAddrBits))

  val est_stride = Reg(UInt())

  val cpu_req_pc_match = Wire(Bool())


  io.alloc_ready := state === s_idle
  io.state_idle := state === s_idle
  io.state_trans := state === s_trans
  io.state_steady := state === s_steady

  cpu_req_pc_match := io.cpu_req_valid && io.cpu_mem_pc === rpte_pc
  io.cpu_req_pc_match := cpu_req_pc_match && state =/= s_idle


  when (cpu_req_pc_match) {
    rpte_counter.value := 0
  }

  when (state === s_idle) {
    when (io.alloc_do && io.cpu_req_valid) {
      state := s_trans
      rpte_pc := io.cpu_mem_pc
      rpte_addr := io.cpu_req_addr
      rpte_stride := 0
    }
  }

  when (state === s_trans) {
    when (cpu_req_pc_match) {
      est_stride := io.cpu_req_addr - rpte_addr
      rpte_addr := io.cpu_req_addr
      when (est_stride === rpte_stride) {
        // correct, go to steady
        state := s_steady
      }
      rpte_stride := est_stride
    }
  }

  when (state === s_steady) {
    when (cpu_req_pc_match) {
      rpte_predict := io.cpu_req_addr + rpte_stride

      est_stride := io.cpu_req_addr - rpte_addr
      rpte_addr := io.cpu_req_addr

      when (est_stride =/= rpte_stride) {
        // incorrect, go to transient
        state := s_trans
        rpte_stride := est_stride
      }
    }
  }

  when (RegNext(cpu_req_pc_match && state === s_steady)) {
    io.prefetch_valid := true.B
    io.prefetch_addr := rpte_predict
  } .otherwise {
    io.prefetch_valid := false.B
  }

  when (rpte_counter_wrap) {
    state := s_idle
  }

}



class RPTFile(tableSize: Int)(p: Parameters) extends L1HellaCacheModule()(p) {
  val io = new Bundle {
    val cpu_req_valid  = Input(Bool())
    val cpu_req_addr   = Input(UInt(width = coreMaxAddrBits))

    val cpu_mem_pc     = Input(UInt(width = 64))

    val prefetch_req_addr   = Output(UInt(width = coreMaxAddrBits))
    val prefetch_req_valid = Output(Bool())
  }

  val req_valid = RegInit(false.B)
  val req_addr  = Reg(UInt(width = coreMaxAddrBits))

  var pc_match = Bool(false)

  val alloc_arb = Module(new Arbiter(Bool(), tableSize))
  val pref_arb = Module(new Arbiter(UInt(width = coreMaxAddrBits), tableSize))

  val RPTEs = (0 until tableSize) map { i =>
    val rpte = Module(new RPTEntry(i)(p))

    rpte.io.cpu_req_valid := io.cpu_req_valid
    rpte.io.cpu_req_addr := io.cpu_req_addr
    rpte.io.cpu_mem_pc := io.cpu_mem_pc

    pref_arb.io.in(i).valid := rpte.io.prefetch_valid
    pref_arb.io.in(i).bits := rpte.io.prefetch_addr

    alloc_arb.io.in(i).valid := rpte.io.alloc_ready
    rpte.io.alloc_do := alloc_arb.io.in(i).ready

    pc_match = pc_match || rpte.io.cpu_req_pc_match

    rpte
  }

  alloc_arb.io.out.ready := io.cpu_req_valid && !pc_match

  io.prefetch_req_valid := pref_arb.io.out.valid
  io.prefetch_req_addr := pref_arb.io.out.bits
}

class StridePrefetcher(implicit p: Parameters) extends NBDCachePrefetcher()(p) {
  override lazy val module = new StridePrefetcherImpl(this)
}

class StridePrefetcherImpl(outer: StridePrefetcher) extends NBDCachePrefetcherImpl(outer) {

  val req_valid = RegInit(false.B)
  var addr_valid = Bool()

  val rpts = Module(new RPTFile(8)(outer.p))


  addr_valid = io.cpu_mem_pc > UInt(0x80000000L, width = 64) && io.cpu_req_addr > UInt(0x80000000L, width = 64)

  rpts.io.cpu_req_valid := RegNext(io.cpu_req_valid && addr_valid)
  rpts.io.cpu_req_addr := RegNext(io.cpu_req_addr)

  rpts.io.cpu_mem_pc := io.cpu_mem_pc



  // Handshake
  val req_addr = RegEnable(rpts.io.prefetch_req_addr, rpts.io.prefetch_req_valid)

  when (rpts.io.prefetch_req_valid) {
    req_valid := true.B
  } .elsewhen(io.prefetch_req.fire()) {
    req_valid := false.B
  }

  io.prefetch_req.valid            := req_valid
  io.prefetch_req.bits.addr        := req_addr
  io.prefetch_req.bits.cmd         := M_PFW
  io.prefetch_req.bits.data        := UInt(0)

}
