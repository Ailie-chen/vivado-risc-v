//******************************************************************************
// See LICENSE.Berkeley for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.lsu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import freechips.rocketchip.rocket._

import boom.common._
import boom.exu.BrResolutionInfo
import boom.util.{IsKilledByBranch, GetNewBrMask, BranchKillableQueue, IsOlder, UpdateBrMask}


// add ailie
class HyperionIO(implicit val p: Parameters) extends Bundle with boom.common.HasBoomCoreParameters{
  val pc_full = UInt(vaddrBitsExtended.W)
  val vaddr = UInt((vaddrBits+1).W)
  val ts = UInt((p(PCPrefHisCfg).HisTsWidth).W)
  val lat = UInt((p(L1DPrefCfg).LatWidth).W)

  def default(): HyperionIO = {
    val w = Wire(new HyperionIO)
    w.pc_full := 0.U(vaddrBitsExtended.W)
    w.vaddr := 0.U((vaddrBits+1).W)
    w.ts := 0.U((p(PCPrefHisCfg).HisTsWidth).W)
    w.lat := 0.U((p(L1DPrefCfg).LatWidth).W)
    w
  }
}


class HyperionIO_addHis(implicit p: Parameters) extends Bundle {
  val base = new HyperionIO() 
  val addHis = Bool()  

  override def cloneType: this.type = new HyperionIO_addHis()(p).asInstanceOf[this.type]

  // 提供默认值
  def default(): HyperionIO_addHis = {
    val w = Wire(new HyperionIO_addHis)
    w.base := (new HyperionIO()).default()
    w.addHis := false.B
    w
  }
}

class HyperionIO_getHis(implicit p: Parameters) extends Bundle {
  val base = new HyperionIO() 
  val getHis = Bool()  

  override def cloneType: this.type = new HyperionIO_getHis()(p).asInstanceOf[this.type]

  // 提供默认值
  def default(): HyperionIO_getHis = {
    val w = Wire(new HyperionIO_getHis)
    w.base := (new HyperionIO()).default()
    w.getHis := false.B
    w
  }
}

class HyperionIO_addDelta(implicit p: Parameters) extends Bundle {
  val base = new HyperionIO() 
  val addDelta = Bool()  

  override def cloneType: this.type = new HyperionIO_addDelta()(p).asInstanceOf[this.type]

  // 提供默认值
  def default(): HyperionIO_addDelta = {
    val w = Wire(new HyperionIO_addDelta)
    w.base := (new HyperionIO()).default()
    w.addDelta := false.B
    w
  }
}

class HyperionIO_getDelta(implicit p: Parameters) extends Bundle {
  val base = new HyperionIO() 
  val getDelta = Bool()  

  override def cloneType: this.type = new HyperionIO_getDelta()(p).asInstanceOf[this.type]

   def default(): HyperionIO_getDelta = {
    val w = Wire(new HyperionIO_getDelta)
    w.base := (new HyperionIO()).default()
    w.getDelta := false.B
    w
  }
}

class PrefAddrReg(implicit val p: Parameters) extends Bundle with boom.common.HasBoomCoreParameters {
  val deltaData = new DeltaData() 
  val deltaNums = UInt((log2Ceil(p(PrefDelCfg).DelNum) + 1).W)
  val base_vaddr = UInt((vaddrBits+1).W)  
  val cmd = UInt(M_SZ.W)
  val timestamp = UInt((p(L1DPrefCfg).TsCntWidth).W)

  override def cloneType: this.type = new PrefAddrReg()(p).asInstanceOf[this.type]
   def default(): PrefAddrReg = {
    val w = Wire(new PrefAddrReg)
    w.deltaData := (new DeltaData()).default
    w.deltaNums := 0.U(((log2Ceil(p(PrefDelCfg).DelNum) + 1).W))
    w.base_vaddr := 0.U((vaddrBits+1).W)
    w.cmd := 0.U(M_SZ.W)
    w.timestamp := 0.U((p(L1DPrefCfg).TsCntWidth).W)
    w
  }
}

class PrefQueueEle(implicit val p: Parameters) extends Bundle with boom.common.HasBoomCoreParameters {
  val prefAddr = UInt(coreMaxAddrBits.W) 
  val timestamp = UInt((p(PCPrefHisCfg).HisTsWidth).W) 
  val req_cmd = UInt(M_SZ.W)
}

class PrefQueueClear[T <: PrefQueueEle](gen: T, entries: Int )(implicit p: Parameters) extends BoomModule()(p)
{
  val io = IO(new Bundle {
    val enq     = Flipped(Decoupled(gen))
    val deq     = Decoupled(gen)
    val empty   = Output(Bool())
    val full    = Output(Bool())
  })

  val ram     = Mem(entries, gen)
  val valids  = RegInit(VecInit(Seq.fill(entries) {false.B}))
  val uops    = Reg(Vec(entries, new MicroOp))

  val enq_ptr = Counter(entries)
  val deq_ptr = Counter(entries)
  val maybe_full = RegInit(false.B)

  val ptr_match = enq_ptr.value === deq_ptr.value
  io.empty := ptr_match && !maybe_full
  val full = ptr_match && maybe_full
  val do_enq = WireInit(io.enq.fire())
  val do_deq = WireInit((io.deq.ready || !valids(deq_ptr.value)) && !io.empty)

  val cycle_counter = RegInit(0.U(log2Ceil(8 * entries + 1).W))
  cycle_counter := cycle_counter + 1.U

  when (cycle_counter === (8 * entries).U) {
    for (i <- 0 until entries) {
      valids(i) := false.B
    }
    cycle_counter := 0.U
    enq_ptr.value := 0.U
    deq_ptr.value := 0.U
    maybe_full := false.B
  }

  when (do_enq) {
    ram(enq_ptr.value)          := io.enq.bits
    valids(enq_ptr.value)       := true.B
    enq_ptr.inc()
  }

  when (do_deq) {
    valids(deq_ptr.value) := false.B
    deq_ptr.inc()
  }

  when (do_enq =/= do_deq) {
    maybe_full := do_enq
  }.elsewhen (do_deq) {
    maybe_full := false.B
  }

  io.enq.ready := !full
  val out = Wire(gen)
  out                     := ram(deq_ptr.value)
  io.deq.valid            := !io.empty && valids(deq_ptr.value)
  io.deq.bits             := out
}

class PrefQueue[T <: Data](gen: T, entries: Int )(implicit p: Parameters)  extends BoomModule()(p)
{
  val io = IO(new Bundle {
    val enq     = Flipped(Decoupled(gen))
    val deq     = Decoupled(gen)
    val empty   = Output(Bool())
    val full    = Output(Bool())
  })

  val ram     = Mem(entries, gen)
  val valids  = RegInit(VecInit(Seq.fill(entries) {false.B}))
  val uops    = Reg(Vec(entries, new MicroOp))

  val enq_ptr = Counter(entries)
  val deq_ptr = Counter(entries)
  val maybe_full = RegInit(false.B)

  val ptr_match = enq_ptr.value === deq_ptr.value
  io.empty := ptr_match && !maybe_full
  val full = ptr_match && maybe_full
  val do_enq = WireInit(io.enq.fire())
  val do_deq = WireInit((io.deq.ready || !valids(deq_ptr.value)) && !io.empty)


  when (do_enq) {
    ram(enq_ptr.value)          := io.enq.bits
    valids(enq_ptr.value)       := true.B 
    enq_ptr.inc()
  }

  when (do_deq) {
    valids(deq_ptr.value) := false.B
    deq_ptr.inc()
  }

  when (do_enq =/= do_deq) {
    maybe_full := do_enq
  }.elsewhen (do_deq) {
    maybe_full := false.B
  }

  io.enq.ready := !full
  val out = Wire(gen)
  out                     := ram(deq_ptr.value)
  io.deq.valid            := !io.empty && valids(deq_ptr.value)
  io.deq.bits             := out
}


abstract class DataPrefetcher(implicit edge: TLEdgeOut, p: Parameters) extends BoomModule()(p)
{
  val io = IO(new Bundle {
    val mshr_avail = Input(Bool())
    val req_val    = Input(Bool())
    val req_addr   = Input(UInt(coreMaxAddrBits.W))
    val req_coh    = Input(new ClientMetadata)
    val prefetch   = Decoupled(new BoomDCacheReq)
    val prefCtrl_addHis = Input(new HyperionIO_addHis)
    val prefCtrl_getHis = Input(new HyperionIO_getHis)
    val prefCtrl_getDelta = Input(new HyperionIO_getDelta)
    val time_cycle = Input(UInt((p(L1DPrefCfg).TsCntWidth).W))

    val tlb_req = Output(new PrefTLBReq)  // TLB请求
    val tlb_resp = Input( new PrefTLBResp)  // TLB响应
    val tlb_valid = Input(Bool())
    val tlb_req_valid = Output(Bool())
    val prefetch_enable = Input(Bool())
  })
}



/**
  * Does not prefetch
  */
class NullPrefetcher(implicit edge: TLEdgeOut, p: Parameters) extends DataPrefetcher
{
  io.prefetch.valid := false.B
  io.prefetch.bits  := DontCare
}

/**
  * Next line prefetcher. Grabs the next line on a cache miss
  */
class NLPrefetcher(implicit edge: TLEdgeOut, p: Parameters) extends DataPrefetcher
{

  val req_valid = RegInit(false.B)
  val req_addr  = Reg(UInt(coreMaxAddrBits.W))
  //ailie:
  // val req_vaddr = Reg(UInt((vaddrBits+1).W))
  val req_cmd   = Reg(UInt(M_SZ.W))
  val cycle_cnt = RegInit(UInt(12.W), 0.U)
  val mshr_req_addr   = io.req_addr + cacheBlockBytes.U
  //ailie
  // val mshr_req_vaddr  = ((io.req_vaddr + cacheBlockBytes.U) >> lgCacheBlockBytes.U) << lgCacheBlockBytes.U
  val cacheable = edge.manager.supportsAcquireBSafe(mshr_req_addr, lgCacheBlockBytes.U)
  when (io.req_val && cacheable) {
    // val pc_full_value = io.pc_full.getOrElse(0.U)
    // printf("********prefetcher*********pc is %d, triger addr is %d, prefetch addr is %d, current cycle is %d \n", pc_full_value, io.req_addr, mshr_req_addr, cycle_cnt)
    req_valid := true.B
    req_addr  := mshr_req_addr
    // req_vaddr := mshr_req_vaddr
    req_cmd   := Mux(ClientStates.hasWritePermission(io.req_coh.state), M_PFW, M_PFR)
  } .elsewhen (io.prefetch.fire()) {
    req_valid := false.B
  }

  cycle_cnt := cycle_cnt + 1.U
  io.prefetch.valid            := req_valid && io.mshr_avail
  io.prefetch.bits.addr        := req_addr
  // io.prefetch.bits.vaddr       := req_vaddr
  io.prefetch.bits.uop         := NullMicroOp
  io.prefetch.bits.uop.mem_cmd := req_cmd
  io.prefetch.bits.data        := DontCare


}


class HyperionPrefetcher(implicit edge: TLEdgeOut, p: Parameters) extends DataPrefetcher
{

  val pcht = Module(new PCHistoryTable(p(PCPrefHisCfg).keyWidth,p(PCPrefHisCfg).numSets,
                              p(PCPrefHisCfg).numWays, p(PCPrefHisCfg).HisDataWidth,
                              p(PCPrefHisCfg).tagWidth, p(PCPrefHisCfg).HisNum,
                              p(PCPrefHisCfg).HisAdrWidth, p(PCPrefHisCfg).HisTsWidth))
  val pcdt = Module(new PCDeltaTable(p(PrefDelCfg).keyWidth,p(PrefDelCfg).numSets,
                            p(PrefDelCfg).numWays, p(PrefDelCfg).DelDataWidth,
                            p(PrefDelCfg).tagWidth, p(PrefDelCfg).DelTcWidth,p(PrefDelCfg).DelNum,
                            p(PCPrefHisCfg).HisNum,p(PrefDelCfg).DelWidth, p(PrefDelCfg).DelLcWidth,
                            p(PrefDelCfg).PCConfMax, p(PrefDelCfg).PageConfMax))

  val prefCtrl_addDelta = Wire(new HyperionIO_addDelta)
  prefCtrl_addDelta.base := io.prefCtrl_getHis.base
  prefCtrl_addDelta.addDelta := io.prefCtrl_getHis.getHis

  //connect wire signal fpr pcht
  pcht.io.rkey        := hash_pc(io.prefCtrl_getHis.base.pc_full)
  pcht.io.addr        := ((io.prefCtrl_addHis.base.vaddr) >> lgCacheBlockBytes.U)(p(PCPrefHisCfg).HisAdrWidth - 1, 0)
  pcht.io.rvalid      := io.prefCtrl_getHis.getHis
  pcht.io.timestamp   := io.prefCtrl_addHis.base.ts
  pcht.io.wvalid      := io.prefCtrl_addHis.addHis
  pcht.io.wkey        := hash_pc(io.prefCtrl_addHis.base.pc_full)

  //compute the offsets
  val prefCtrl_getHis_r1 = RegNext(io.prefCtrl_getHis)
  val HisSortentry = Wire(Vec(p(PCPrefHisCfg).HisNum, new HistoryEle()(p))) 
  val isHead0 = (pcht.io.data_entry.His(pcht.io.data_entry.head).address === 0.U)
  //sort the array by the access time
  val sortStart = Mux(isHead0.asBool, 0.U, pcht.io.data_entry.head)
  for(i <- 0 until (p(PCPrefHisCfg).HisNum)){
    HisSortentry(i) := pcht.io.data_entry.His((i.U + sortStart)%((p(PCPrefHisCfg).HisNum).U))
  }
  // find the postion, which is Older
  val OlderVec = VecInit((0 until p(PCPrefHisCfg).HisNum).map { i =>
      (HisSortentry(i).timestamp > (prefCtrl_getHis_r1.base.ts - prefCtrl_getHis_r1.base.lat)) ||
      (HisSortentry(i).timestamp === 0.U && HisSortentry(i).address === 0.U )})
  val isOlder = OlderVec.reduce(_ || _)
  val isOlderPos = Mux(isOlder, PriorityEncoder(OlderVec), (p(PCPrefHisCfg).HisNum).U)
  //find the postion, which has same address
  val PosAdrEquVec = VecInit((0 until p(PCPrefHisCfg).HisNum).map { i =>
     (HisSortentry(i).address === (prefCtrl_getHis_r1.base.vaddr >> lgCacheBlockBytes.U)(p(PCPrefHisCfg).HisAdrWidth - 1, 0)) && (!OlderVec(i))})                                                                           
  val isPosAdrEqu = PosAdrEquVec.reduce(_ || _)
  val isAdrEquPos = Mux(isPosAdrEqu, PriorityEncoder(PosAdrEquVec), (p(PCPrefHisCfg).HisNum).U)
  val DeltaEntry = WireInit(new DeltaEleHis()(p).default)

  for(i <-0 until p(PCPrefHisCfg).HisNum)
  {
    // 先确保两个操作数位宽相同
    val addr1 = (prefCtrl_getHis_r1.base.vaddr >> lgCacheBlockBytes.U)(p(PrefDelCfg).DelWidth -2, 0)
    val addr2 = HisSortentry(i).address(p(PrefDelCfg).DelWidth -2, 0)

    // 然后进行有符号减法
    val delta = (addr1.asSInt - addr2.asSInt)

    when( i.U < Mux(isAdrEquPos < isOlderPos, isAdrEquPos,  isOlderPos)){
      DeltaEntry.deltas(i) := delta
    } .elsewhen((i.U >= Mux((isAdrEquPos + 1.U) < isOlderPos,(isAdrEquPos + 1.U), isOlderPos)) && (i.U < isOlderPos) ){
      DeltaEntry.deltas(Mux(isPosAdrEqu, i.U - 1.U, i.U)) := delta
    } .otherwise{
      DeltaEntry.deltas(i) := 0.S
    }
  }

  DeltaEntry.nums := Mux(isPosAdrEqu, isOlderPos - 1.U, isOlderPos)
  

  //connect wire signal for pcdt
  pcdt.io.wkey      := hash_pc(io.prefCtrl_getHis.base.pc_full)
  pcdt.io.offsetIn  := DeltaEntry
  pcdt.io.wvalid    := io.prefCtrl_getHis.getHis
  pcdt.io.rkey      := hash_pc(io.prefCtrl_getDelta.base.pc_full)
  pcdt.io.rvalid    := io.prefCtrl_getDelta.getDelta

  
  val DeltasR = RegInit(VecInit(Seq.fill(p(QCfg).DeltasRegNum)((new PrefAddrReg()).default)))
  val DeltasWCnt = RegInit(0.U((log2Ceil(p(QCfg).DeltasRegNum)).W))
  val DeltasRCnt = RegInit(0.U((log2Ceil(p(QCfg).DeltasRegNum)).W))
  val addPQPtr = RegInit(0.U((log2Ceil(p(PrefDelCfg).DelNum)).W))
  when(RegNext(io.prefCtrl_getDelta.getDelta) && (pcdt.io.offsetsNumOut =/= 0.U))
  {

    DeltasR(DeltasWCnt).deltaData := pcdt.io.offsetsOut
    DeltasR(DeltasWCnt).deltaNums := pcdt.io.offsetsNumOut
    DeltasR(DeltasWCnt).base_vaddr := RegNext(io.prefCtrl_getDelta.base.vaddr)
    // DeltasR(DeltasWCnt).cmd := Mux(ClientStates.hasWritePermission(RegNext(io.req_coh.state)), M_PFW, M_PFR)
    DeltasR(DeltasWCnt).timestamp := io.time_cycle
    DeltasWCnt := (DeltasWCnt + 1.U)%((p(QCfg).DeltasRegNum).U)
  }
  
  val pq = Module(new PrefQueueClear(new PrefQueueEle, p(QCfg).PrefNum))
  val Currentdelta = DeltasR(DeltasRCnt).deltaData.deltas(addPQPtr).delta
  val MSBDelta = (Currentdelta(p(PrefDelCfg).DelWidth - 1, p(PrefDelCfg).DelWidth - 1)).asBool
  val AlgOnes = Fill(p(PrefDelCfg).DelWidth - 1, true.B)
  val DeltaMod = Mux(MSBDelta, (Currentdelta(p(PrefDelCfg).DelWidth - 2, 0)^AlgOnes).asUInt + 1.U, Currentdelta(p(PrefDelCfg).DelWidth - 2, 0).asUInt)
  val DeltaMod_block = DeltaMod << lgCacheBlockBytes.U
  val base_vaddr = DeltasR(DeltasRCnt).base_vaddr
  val readyPrefAdr =  Mux(MSBDelta, 
  Mux(base_vaddr  > DeltaMod_block, base_vaddr - DeltaMod_block, base_vaddr),
  (base_vaddr + DeltaMod_block))
  // 判断预取后的地址和触发地址在一个页内
  val pref_addr_in_page = (readyPrefAdr(39, 12) === DeltasR(DeltasRCnt).base_vaddr(39, 12))
  val is_conf = (((DeltasR(DeltasRCnt).deltaData.deltas(addPQPtr).localCnt << 8.U).asUInt() / DeltasR(DeltasRCnt).deltaData.totalCnt) >= (p(L1DPrefCfg).PrefThresh).U) && (DeltasR(DeltasRCnt).deltaNums =/= 0.U) && (DeltasR(DeltasRCnt).deltaData.totalCnt >= 4.U)
  pq.io.enq.valid := is_conf && pref_addr_in_page

  
  pq.io.enq.bits.timestamp := DeltasR(DeltasRCnt).timestamp
  pq.io.enq.bits.req_cmd := DeltasR(DeltasRCnt).cmd
  pq.io.enq.bits.prefAddr := readyPrefAdr
  when((pq.io.enq.ready) && (addPQPtr < DeltasR(DeltasRCnt).deltaNums) ){
    when( addPQPtr === (DeltasR(DeltasRCnt).deltaNums - 1.U)){
      addPQPtr := 0.U
      DeltasRCnt := (DeltasRCnt + 1.U)%((p(QCfg).DeltasRegNum).U)
      when((DeltasWCnt =/= DeltasRCnt) || (RegNext(io.prefCtrl_getDelta.getDelta)===false.B)){
        DeltasR(DeltasRCnt).deltaNums := 0.U
      }
    } .otherwise{
      addPQPtr := addPQPtr + 1.U
    }
  }



  //connetc tlb signals to pq
  pq.io.deq.ready := io.tlb_valid 
  io.tlb_req_valid := pq.io.deq.valid && (pq.io.deq.bits.prefAddr =/= 0.U) && io.prefetch_enable
  io.tlb_req.vaddr := pq.io.deq.bits.prefAddr
  io.prefetch.valid := false.B
  //reg the prefetch signals when tlb_valid is true
  val pref_valid = RegInit(false.B)
  val pref_addr = RegInit(0.U((coreMaxAddrBits).W))
  val pref_data = RegInit(0.U(coreDataBits.W))
  
  // 处理TLB响应
  when (io.tlb_valid) {
    // when (io.tlb_resp.miss || io.tlb_resp.pf.ld || io.tlb_resp.ae.ld || (!io.tlb_resp.cacheable) || (!io.tlb_resp.prefetchable)) {
    when (io.tlb_resp.miss || (!io.tlb_resp.cacheable) || (!io.tlb_resp.prefetchable)) {
      // 静默丢弃，不发送预取请求
      io.prefetch.valid := false.B
    } .otherwise {
      // 地址有效且有权限，发送预取请求
      pref_valid := true.B
      pref_addr := io.tlb_resp.paddr
      pref_data := Cat(0.U((coreDataBits - p(L1DPrefCfg).TsCntWidth).W),
                        io.time_cycle(p(L1DPrefCfg).TsCntWidth - 1, 0))
    }
  }


  val cacheable = edge.manager.supportsAcquireBSafe(io.tlb_resp.paddr, lgCacheBlockBytes.U)


  // val low_phy_adr = 0x0080200000
  // val high_phy_adr = 0x00ffffffff
  // val low_phy_adr = 2050.U
  // val high_phy_adr = 4095.U
  val low_phy_adr = 2048.U(12.W)
  val high_phy_adr = 4095.U(12.W)

  // 如果物理地址在0x0080200000-0x00ffffffff之间，则不发送预取请求，不过通过截取[31:20]位，判断其大于0x802, 小于0xfff
  // 并且[39:32]位为0x00
  val pref_valid_mask = (pref_addr(39, 32) === 0.U(8.W)) && (pref_addr(31, 20) >= low_phy_adr) && (pref_addr(31, 20 ) < high_phy_adr)

  io.prefetch.valid := cacheable && io.mshr_avail && pref_valid && pref_valid_mask && io.prefetch_enable
  // io.prefetch.valid := false.B

  io.prefetch.bits.addr := pref_addr
  io.prefetch.bits.uop         := NullMicroOp
  io.prefetch.bits.uop.mem_cmd := M_PFR  // 预取读请求
  io.prefetch.bits.data        := pref_data

  when(pref_valid && io.prefetch.ready) {
    pref_valid := false.B
  }

  def hash_pc(pc: UInt)(implicit p: Parameters): UInt = {
    ((pc >> 1) ^ (pc >> 4))&((1.U << (p(PCPrefHisCfg).keyWidth).U) - 1.U)
  }

}