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



abstract class DataPrefetcher(implicit edge: TLEdgeOut, p: Parameters) extends BoomModule()(p)
{
  val io = IO(new Bundle {
    val mshr_avail = Input(Bool())
    val req_val    = Input(Bool())
    val req_addr   = Input(UInt(coreMaxAddrBits.W))
    //ailie:
    val req_vaddr   = Input(UInt((vaddrBits+1).W))
    val req_coh    = Input(new ClientMetadata)
    //add by ailie
    val pc_full = if(p(HyperionDefKey)) Some(Input(UInt(vaddrBitsExtended.W))) else None
    val prefetch   = Decoupled(new BoomDCacheReq)
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
  val req_vaddr = Reg(UInt((vaddrBits+1).W))
  val req_cmd   = Reg(UInt(M_SZ.W))
  val cycle_cnt = RegInit(UInt(12.W), 0.U)
  val mshr_req_addr   = io.req_addr + cacheBlockBytes.U
  //ailie
  val mshr_req_vaddr  = ((io.req_vaddr + cacheBlockBytes.U) >> lgCacheBlockBytes.U) << lgCacheBlockBytes.U
  val cacheable = edge.manager.supportsAcquireBSafe(mshr_req_addr, lgCacheBlockBytes.U)
  when (io.req_val && cacheable) {
    val pc_full_value = io.pc_full.getOrElse(0.U)
    printf("********prefetcher*********pc is %d, triger addr is %d, prefetch addr is %d, current cycle is %d \n", pc_full_value, io.req_addr, mshr_req_addr, cycle_cnt)
    req_valid := true.B
    req_addr  := mshr_req_addr
    req_vaddr := mshr_req_vaddr
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

  
  
  val req_valid = RegInit(false.B)
  val req_addr  = Reg(UInt(coreMaxAddrBits.W))
  //ailie:
  val req_vaddr = Reg(UInt((vaddrBits+1).W))
  val req_cmd   = Reg(UInt(M_SZ.W))
  val cycle_cnt = RegInit(UInt(12.W), 0.U)

  val pcHisEntry = RegInit({
    val pchData = Wire(new PCHistoryData()(p)) // 使用 Wire 来定义硬件类型
    pchData.His.zipWithIndex.foreach { case (ele, idx) =>
      ele.address := RegInit(0.U(p(PCPrefHisCfg).HisAdrWidth.W))  // address 字段初始化为0
      ele.timestamp := RegInit(0.U(p(PCPrefHisCfg).HisTsWidth.W))  // timestamp 字段初始化为0
    }
    pchData.head := RegInit(0.U((log2Ceil(p(PCPrefHisCfg).HisNum)).W))
    pchData
  })

  val req_val_r = RegInit(VecInit(Seq.fill(3)(false.B)))
  val key_r = RegInit(VecInit(Seq.fill(3)(0.U((p(PCPrefHisCfg).keyWidth).W))))
  val addr_r = RegInit(VecInit(Seq.fill(3)(0.U((p(PCPrefHisCfg).keyWidth).W))))
  for(i <- 0 until 2){
    req_val_r(i+1) := req_val_r(i)
  }
  for(i <- 0 until 2){
    key_r(i+1) := key_r(i)
    addr_r(i+1) := addr_r(i)
  }

  val pc_key = io.pc_full match {
    case Some(pc) => pc
    case None => 0.U((p(PCPrefHisCfg).keyWidth).W)
  }
  val pcht = Module(new PCHistoryTable(p(PCPrefHisCfg).keyWidth,p(PCPrefHisCfg).numSets,
                              p(PCPrefHisCfg).numWays, p(PCPrefHisCfg).HisDataWidth,
                              p(PCPrefHisCfg).tagWidth, p(PCPrefHisCfg).HisNum,
                              p(PCPrefHisCfg).HisAdrWidth, p(PCPrefHisCfg).HisTsWidth))
  when(io.req_val){
    pcht.io.key := ((pc_key >> 1) ^ (pc_key >> 4))
    pcht.io.wvalid := true.B
    pcht.io.timestamp := cycle_cnt 
    pcht.io.addr := io.req_addr
    req_val_r(0) := true.B
  } .otherwise{
    pcht.io.key := 0.U((p(PCPrefHisCfg).keyWidth).W)
    pcht.io.wvalid := false.B
    pcht.io.timestamp := 0.U(12.W)
    pcht.io.addr := 0.U((p(PCPrefHisCfg).HisAdrWidth).W)
    req_val_r(0) := false.B
  }
  when(req_val_r(1)){
    pcHisEntry := pcht.io.data_entry
    pcht.io.rvalid := true.B
    pcht.io.key := key_r(1)
    pcht.io.addr := addr_r(1)
  } .otherwise{
    pcht.io.rvalid := false.B
  }

  val mshr_req_addr   = Mux(pcHisEntry.His(0).address === 0.U, io.req_addr + cacheBlockBytes.U, io.req_addr +  cacheBlockBytes.U + cacheBlockBytes.U)
  //ailie
  val mshr_req_vaddr  = ((io.req_vaddr + cacheBlockBytes.U) >> lgCacheBlockBytes.U) << lgCacheBlockBytes.U
  val cacheable = edge.manager.supportsAcquireBSafe(mshr_req_addr, lgCacheBlockBytes.U)

  
  when (io.req_val && cacheable) {
  // when (req_val_r(2) && cacheable) {
    printf("********Hyperion*********triger addr is %d,prefetch addr is %d, current cycle is %d \n", io.req_addr, mshr_req_addr,cycle_cnt)
    req_valid := true.B
    req_addr  := mshr_req_addr
    req_vaddr := mshr_req_vaddr
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


class Hyperion1Prefetcher(implicit edge: TLEdgeOut, p: Parameters) extends DataPrefetcher
{

  
  
  val req_valid = RegInit(false.B)
  val req_addr  = Reg(UInt(coreMaxAddrBits.W))
  //ailie:
  val req_vaddr = Reg(UInt((vaddrBits+1).W))
  val req_cmd   = Reg(UInt(M_SZ.W))
  val cycle_cnt = RegInit(UInt(12.W), 0.U)

  val pcHisEntry = RegInit({
    val pchData = Wire(new PCHistoryData()(p)) // 使用 Wire 来定义硬件类型
    pchData.His.zipWithIndex.foreach { case (ele, idx) =>
      ele.address := RegInit(0.U(p(PCPrefHisCfg).HisAdrWidth.W))  // address 字段初始化为0
      ele.timestamp := RegInit(0.U(p(PCPrefHisCfg).HisTsWidth.W))  // timestamp 字段初始化为0
    }
    pchData.head := RegInit(0.U((log2Ceil(p(PCPrefHisCfg).HisNum)).W))
    pchData
  })

  val req_val_r = RegInit(VecInit(Seq.fill(3)(false.B)))
  val key_r = RegInit(VecInit(Seq.fill(3)(0.U((p(PCPrefHisCfg).keyWidth).W))))
  val addr_r = RegInit(VecInit(Seq.fill(3)(0.U((p(PCPrefHisCfg).keyWidth).W))))


  val PcDebug = Reg(Vec(256, UInt(p(PCPrefHisCfg).keyWidth.W)))
  val RdDebug = RegInit(0.U(8.W))
  val WrDebug = RegInit(0.U(8.W))
  val AdrDebug = Reg(Vec(256, UInt(p(PCPrefHisCfg).HisAdrWidth.W)))
  val WawHzd_cnt = RegInit(0.U(8.W))

  when(reset.asBool) {
     for (i <- 0 until 256) {
      PcDebug(i) := (i & 0xf).asUInt.max(1.U).min(16.U)
    }
    for (i <- 0 until 256) {
      AdrDebug(i) := (i & 0x3f).asUInt.max(1.U).min(32.U)
    }
  }

  for(i <- 0 until 2){
    req_val_r(i+1) := req_val_r(i)
    key_r(i+1) := key_r(i)
    addr_r(i+1) := addr_r(i)
  }

  val pc_key = io.pc_full match {
    case Some(pc) => pc
    case None => 0.U((p(PCPrefHisCfg).keyWidth).W)
  }
  val pcht = Module(new PCHistoryTable(p(PCPrefHisCfg).keyWidth,p(PCPrefHisCfg).numSets,
                              p(PCPrefHisCfg).numWays, p(PCPrefHisCfg).HisDataWidth,
                              p(PCPrefHisCfg).tagWidth, p(PCPrefHisCfg).HisNum,
                              p(PCPrefHisCfg).HisAdrWidth, p(PCPrefHisCfg).HisTsWidth))
  when(io.req_val && RdDebug =/= 90.U){
    pcht.io.key := PcDebug(RdDebug)
    pcht.io.wvalid := true.B
    pcht.io.timestamp := cycle_cnt 
    pcht.io.addr := AdrDebug(RdDebug)
    req_val_r(0) := true.B
    key_r(0) := PcDebug(RdDebug)
    RdDebug := RdDebug + 1.U
  } .elsewhen(RdDebug === 90.U && req_val_r(0)){
    pcht.io.key := PcDebug(RdDebug - 1.U)
    pcht.io.wvalid := true.B
    pcht.io.timestamp := cycle_cnt 
    pcht.io.addr := AdrDebug(RdDebug)
    req_val_r(0) := false.B
    RdDebug := RdDebug
  } .elsewhen(RdDebug === 90.U && req_val_r(1)){
    pcht.io.key := PcDebug(RdDebug - 1.U)
    pcht.io.wvalid := true.B
    pcht.io.timestamp := cycle_cnt 
    pcht.io.addr := AdrDebug(RdDebug)
    RdDebug := RdDebug + 1.U
  }.otherwise{
    pcht.io.key := 0.U((p(PCPrefHisCfg).keyWidth).W)
    pcht.io.wvalid := false.B
    pcht.io.timestamp := 0.U(12.W)
    pcht.io.addr := 0.U((p(PCPrefHisCfg).HisAdrWidth).W)
    req_val_r(0) := false.B
    RdDebug := RdDebug
  }

  when(RdDebug === 2.U && io.req_val){
    printf("***when RdDebug = 1.U, the value of PCDebug is %d \n", PcDebug(1))
  }

  when((RdDebug === 100.U || RdDebug === 190.U )&& req_val_r(0) && !(RdDebug === 90.U && req_val_r(1))){
    pcHisEntry := pcht.io.data_entry
    pcht.io.rvalid := true.B
    pcht.io.key := key_r(0)
    pcht.io.addr := addr_r(0)
    WrDebug := WrDebug + 1.U
  } .elsewhen((RdDebug =/= 100.U && RdDebug =/= 190.U)&& req_val_r(1)){
    pcHisEntry := pcht.io.data_entry
    pcht.io.rvalid := true.B
    pcht.io.key := key_r(1)
    pcht.io.addr := addr_r(1)
    WrDebug := WrDebug + 1.U
  } .otherwise{
    pcht.io.rvalid := false.B
  }

  val mshr_req_addr   = Mux(pcHisEntry.His(0).address =/= 0.U, io.req_addr + cacheBlockBytes.U, io.req_addr +  cacheBlockBytes.U + cacheBlockBytes.U)
  //ailie
  val mshr_req_vaddr  = ((io.req_vaddr + cacheBlockBytes.U) >> lgCacheBlockBytes.U) << lgCacheBlockBytes.U
  val cacheable = edge.manager.supportsAcquireBSafe(mshr_req_addr, lgCacheBlockBytes.U)

  
  // when (io.req_val && cacheable) {
  when (req_val_r(2) && cacheable) {
    printf("********Hyperion*********triger addr is %d,prefetch addr is %d, current cycle is %d \n", io.req_addr, mshr_req_addr,cycle_cnt)
    req_valid := true.B
    req_addr  := mshr_req_addr
    req_vaddr := mshr_req_vaddr
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