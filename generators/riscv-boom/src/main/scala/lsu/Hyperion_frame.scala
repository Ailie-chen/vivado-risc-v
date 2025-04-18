
package boom.lsu

import chisel3._
import chisel3.util.{log2Ceil}
import chisel3.util._
import chisel3.util.PopCount

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import freechips.rocketchip.rocket._

import boom.common._
import boom.exu.BrResolutionInfo
import boom.util.{IsKilledByBranch, GetNewBrMask, BranchKillableQueue, IsOlder, UpdateBrMask}

class HistoryEle(implicit p: Parameters) extends Bundle {
  val address = UInt((p(PCPrefHisCfg).HisAdrWidth).W)  // 地址
  val timestamp = UInt((p(PCPrefHisCfg).HisTsWidth).W) // 时间戳
  override def cloneType: this.type = new HistoryEle().asInstanceOf[this.type]
  // **定义一个默认值**
  def default: HistoryEle = {
    val d = Wire(new HistoryEle())
    d.address := 0.U
    d.timestamp := 0.U
    d
  }
}

class PCHistoryData(implicit p: Parameters) extends Bundle {
  val His = Vec(p(PCPrefHisCfg).HisNum, new HistoryEle())
  val head = (UInt((log2Ceil(p(PCPrefHisCfg).HisNum)).W))
  override def cloneType: this.type = new PCHistoryData()(p).asInstanceOf[this.type]
  // **定义一个默认值**
  def default: PCHistoryData = {
    val d = Wire(new PCHistoryData())
    d.His.foreach(_ := (new HistoryEle()).default) // **每个HistoryEle初始化为默认值**
    d.head := 0.U
    d
  }
}

class DeltaEle(implicit p: Parameters) extends Bundle {
  val delta = SInt((p(PrefDelCfg).DelWidth).W)  // 地址
  val localCnt = UInt((p(PrefDelCfg).DelLcWidth).W) // 时间戳
  override def cloneType: this.type = new DeltaEle().asInstanceOf[this.type]
  // **定义一个默认值**
  def default: DeltaEle = {
    val d = Wire(new DeltaEle())
    d.delta := 0.S
    d.localCnt := 0.U
    d
  }
}

class DeltaEleHis(implicit p: Parameters) extends Bundle {
  val deltas = Vec(p(PCPrefHisCfg).HisNum, SInt((p(PrefDelCfg).DelWidth).W))
  val nums = UInt(((log2Ceil(p(PCPrefHisCfg).HisNum)+1).W))
  override def cloneType: this.type = new DeltaEleHis().asInstanceOf[this.type]
  // **定义一个默认值**
  def default: DeltaEleHis = {
    val d = Wire(new DeltaEleHis())
    d.deltas.foreach(_ := 0.S)
    d.nums := 0.U
    d
  }
}

//using: val myCacheLine = Wire(new PrefCacheLine(16, UInt(32.W)))
class DeltaData(implicit p: Parameters) extends Bundle {
  val totalCnt = UInt((p(PrefDelCfg).DelTcWidth).W)
  val deltas = Vec(p(PrefDelCfg).DelNum, new DeltaEle())
  override def cloneType: this.type = new DeltaData()(p).asInstanceOf[this.type]
  // **定义一个默认值**
  def default: DeltaData = {
    val d = Wire(new DeltaData())
    d.deltas.foreach(_ := (new DeltaEle()).default) // **每个HistoryEle初始化为默认值**
    d.totalCnt := 0.U
    d
  }
}



class PTReadPort(val numSets: Int, val numWays: Int, val HisDataWidth: Int) extends Bundle {
  val set  = UInt(log2Ceil(numSets).W)  // 读取的组索引
  val data  = Vec(numWays, UInt(HisDataWidth.W))// 读取的数据
  val enable = Bool()                    // 读使能
}

class PTWritePort(val numSets: Int, val numWays: Int, val HisDataWidth: Int) extends Bundle {
  val set  = UInt(log2Ceil(numSets).W)  // 写入的组索引
  val way   = UInt(log2Ceil(numWays).W)  // 写入的路索引
  val data  = UInt(HisDataWidth.W)          // 写入的数据
  val enable = Bool()                    // 写使能
}






class PCHistoryTable(val keyWidth: Int, val numSets: Int, val numWays: Int, 
val HisDataWidth: Int, val tagWidth: Int, val HisNum: Int, val HisAdrWidth: Int, 
val HisTsWidth: Int)(implicit p: Parameters) extends BoomModule {
  val io = IO(new Bundle {
    val wkey = Input(UInt(keyWidth.W))
    val rkey = Input(UInt(keyWidth.W))
    val addr = Input(UInt(HisAdrWidth.W))
    val timestamp = Input(UInt(HisTsWidth.W))
    val rvalid = Input(Bool())
    val wvalid = Input(Bool())
    val data_entry = Output(new PCHistoryData) 
  })
  val invalidData = Wire(new PCHistoryData()(p))
  invalidData := 0.U.asTypeOf(new PCHistoryData()(p))  // 或者根据实际需要初始化
  
  
  //reset信号，存储在BOOMMOdule中，一般使用when(reset.asBool)来判断
  //*************************存储逻辑*******************************//
  // 定义缓存结构以及初始化
  val dataArray = Seq.fill(numWays)(SyncReadMem(numSets, UInt(HisDataWidth.W)))
  val tagArray   = RegInit(VecInit(Seq.fill(numWays)(VecInit(Seq.fill(numSets)(0.U(tagWidth.W))))))
  val validArray = RegInit(VecInit(Seq.fill(numWays)(VecInit(Seq.fill(numSets)(false.B)))))
  val LruCounters = RegInit(VecInit(Seq.fill(numWays)(VecInit(Seq.fill(numSets)(0.U(log2Ceil(numWays).W))))))
  //初始化
  // when(reset.asBool){
  //   for (way <- 0 until numWays) {
  //     for (set <- 0 until numSets) {
  //       dataArray(way).write(set.U, ((new PCHistoryData()).default).asUInt()) 
  //     }
  //   }
  // }
  // 读写端口
  val readPort = Wire(new PTReadPort(numSets, numWays, HisDataWidth))
  val writePort = Wire(new PTWritePort(numSets, numWays, HisDataWidth))

  // 获取Idx和tag:stage 0
  val rsetIdx = io.rkey(log2Up(numSets)-1, 0)
  val wsetIdx = io.wkey(log2Up(numSets)-1, 0)
  readPort.set := Mux(io.rvalid, rsetIdx, Mux(io.wvalid, wsetIdx, 0.U))
  writePort.set := wsetIdx
  val rtag = io.rkey(tagWidth + log2Up(numSets) - 1, log2Up(numSets))
  val wtag = io.wkey(tagWidth + log2Up(numSets) - 1, log2Up(numSets))

  //寄存器组
   //寄存器组合，从stage0开始， data_r从 stage1开始
  val rvalid_r = RegInit(VecInit(Seq.fill(1)(false.B))) // 4 个 8 位宽的 `UInt`，初始值为 0
  val wvalid_r = RegInit(VecInit(Seq.fill(2)(false.B))) // 4 个 8 位宽的 `UInt`，初始值为 0
  val rkey_r = RegInit(VecInit(Seq.fill(2)(0.U(keyWidth.W))))
  val wkey_r = RegInit(VecInit(Seq.fill(2)(0.U(keyWidth.W))))
  val addr_r = RegInit(VecInit(Seq.fill(1)(0.U(HisAdrWidth.W))))
  val rtag_r = RegInit(VecInit(Seq.fill(1)(0.U(tagWidth.W))))
  val wtag_r = RegInit(VecInit(Seq.fill(1)(0.U(tagWidth.W))))
  val ts_r = RegInit(VecInit(Seq.fill(1)(0.U(HisTsWidth.W))))
  val wdata_r = RegInit(VecInit(Seq.fill(1)({
    val pchData = Wire(new PCHistoryData()(p))
    pchData.His.zipWithIndex.foreach { case (ele, idx) =>
      ele.address := 0.U(p(PCPrefHisCfg).HisAdrWidth.W)
      ele.timestamp := 0.U(p(PCPrefHisCfg).HisTsWidth.W)
    }
    pchData.head := 0.U(log2Ceil(p(PCPrefHisCfg).HisNum).W)
    pchData
  })))
  val writeSet_r = RegInit(VecInit(Seq.fill(1)(0.U(log2Ceil(numSets).W))))
  val writeWay_r = RegInit(VecInit(Seq.fill(1)(0.U(log2Ceil(numWays).W))))
  val rsetIdx_r = RegInit(VecInit(Seq.fill(1)(0.U((log2Ceil(numSets)).W))))
  val wsetIdx_r = RegInit(VecInit(Seq.fill(1)(0.U((log2Ceil(numSets)).W))))
  val rhit_r = RegInit(VecInit(Seq.fill(1)(false.B)))
  val whit_r = RegInit(VecInit(Seq.fill(1)(false.B)))
  val rhitWay_r = RegInit(VecInit(Seq.fill(1)(0.U((log2Ceil(numWays)).W))))
  val whitWay_r = RegInit(VecInit(Seq.fill(1)(0.U((log2Ceil(numWays)).W))))
  val lineToReplace_r = RegInit(VecInit(Seq.fill(1)(0.U((log2Ceil(numWays) + 1).W))))
  val lruIdx_r = RegInit(VecInit(Seq.fill(1)(0.U((log2Ceil(numWays)).W))))
  val RawHzd_r = RegInit(false.B)
  
  
  // 检查是否命:stage 1
  // 这里，在way和index更新后的第二个周期，validArray()()和tagArray()()才会更新相应的值
  // 第一组信号：是否命中，命中的话，数据是多少
  val rwayHits = VecInit((0 until numWays).map { way =>
     validArray(way)(rsetIdx) && (tagArray(way)(rsetIdx) === rtag) && io.rvalid})
  val rhit = rwayHits.reduce(_ || _)
  val wwayHits = VecInit((0 until numWays).map { way =>
     validArray(way)(wsetIdx) && (tagArray(way)(wsetIdx) === wtag && io.wvalid)})
  val whit = wwayHits.reduce(_ || _)
  val rhitWay = Mux(rhit, PriorityEncoder(rwayHits), numWays.asUInt) // 获取命中的way的索引
  val whitWay = Mux(whit, PriorityEncoder(wwayHits), numWays.asUInt) // 获取命中的way的索引
  // 第二组信号(for write):是否存在无效的路,以及LRU的数
  val invalidWay = VecInit((0 until numWays).map { way => 
    !(validArray(way)(wsetIdx))})
  val invalidWayExt = invalidWay.reduce(_ || _)
  val lineToReplace = Mux(invalidWayExt, PriorityEncoder(invalidWay), numWays.asUInt)
  val wayLruPairs = VecInit((0 until numWays).map(way => {
    val pair = Wire(Vec(2, UInt(log2Ceil(numWays).W)))
      pair(0) := LruCounters(way)(wsetIdx)  // LRU 计数
      pair(1) := way.U                      // Way 索引
      pair
    }))
  val lruIdx = wayLruPairs.reduce((a, b) => Mux(a(0) > b(0), a, b))(1)
  // 第三组信号，读出的数据，以及相应的替换的历史
  val RawHzd = ((io.rkey === wkey_r(0)) && wvalid_r(0) && io.rvalid )  
  RawHzd_r := RawHzd
  val WawHzd = (wkey_r(1)===wkey_r(0)) && wvalid_r(0) && wvalid_r(1)

  readPort.enable := Mux(RawHzd, false.B, (io.rvalid || io.wvalid))
  for (way <- 0 until numWays) {
    readPort.data(way) := dataArray(way).read(readPort.set, readPort.enable)}

  // stage 1
  val readData = Mux((RawHzd_r || WawHzd), wdata_r(0),
   Mux(rhit_r(0) , readPort.data(rhitWay_r(0)).asTypeOf(new PCHistoryData()(p)), 
   Mux(whit_r(0), readPort.data(whitWay_r(0)).asTypeOf(new PCHistoryData()(p)), invalidData))) 
  val hisEle = readData.His
  val HisVec = hisEle.map(entry => entry.address === addr_r(0)) // 找到第一个匹配的位置
  val isHisMatch = HisVec.reduce(_ || _)
  val HisMatchIdx = Mux(isHisMatch, PriorityEncoder(HisVec), HisNum.U) // 找不到时返回 HisNum.U
  val HisLruIdx = readData.head

  val wdata = Wire(new PCHistoryData()(p))
  wdata := readData
  when(!(isHisMatch.asBool)){
    wdata.His(readData.head).address := addr_r(0)
    wdata.His(readData.head).timestamp := ts_r(0)
    wdata.head := (readData.head + 1.U) % HisNum.U
  }
  
  

  //相关存储操作：
  //寄存器组合，从setIdx从stage0开始，其他从stage1开始
  rsetIdx_r(0) := rsetIdx
  wsetIdx_r(0) := wsetIdx
  rhit_r(0) := rhit
  whit_r(0) := whit
  rhitWay_r(0) := rhitWay
  whitWay_r(0) := whitWay
  lineToReplace_r(0) := lineToReplace
  lruIdx_r(0) := lruIdx
  for(i <- 0 until 0){
    rsetIdx_r(i+1) := rsetIdx_r(i)
    wsetIdx_r(i+1) := wsetIdx_r(i)
    lineToReplace_r(i+1) := lineToReplace_r(i)
    lruIdx_r(i+1) := lruIdx_r(i)
    rhit_r(i+1) := rhit_r(i)
    whit_r(i+1) := whit_r(i)
    rhitWay_r(i+1) := rhitWay_r(i)
    whitWay_r(i+1) := whitWay_r(i)
  }

  //相关控制操作
  rvalid_r(0) := io.rvalid
  wvalid_r(0) := io.wvalid
  rkey_r(0) := io.rkey
  wkey_r(0) := io.wkey
  addr_r(0) := io.addr
  ts_r(0) := io.timestamp
  wdata_r(0) := wdata
  rtag_r(0) := rtag
  wtag_r(0) := wtag

  for(i <- 0 until 0){
    rvalid_r(i+1) := rvalid_r(i)
    addr_r(i + 1) := addr_r(i)
    ts_r(i + 1) := ts_r(i)
    rtag_r(i + 1) := rtag_r(i)
    wtag_r(i + 1) := wtag_r(i)
  }
  for(i <- 0 until 1){
    wvalid_r(i +1 ) := wvalid_r(i)
    rkey_r(i + 1) := rkey_r(i)
    wkey_r(i + 1) := wkey_r(i)
  }
  for(i <- 0 until 0){
    wdata_r(i + 1) := wdata_r(i)
    writeSet_r(i + 1) := writeSet_r(i)
    writeWay_r(i + 1) := writeWay_r(i)
  }

  
  // stage 1: 取出数据, 判断是否有相同地址等
  writePort.enable := Mux(wvalid_r(0), true.B, false.B)
  writePort.data := wdata.asUInt
  val writeWay = Mux(WawHzd, writeWay_r(0), Mux(whit_r(0), whitWay_r(0).asUInt,Mux(lineToReplace_r(0) === numWays.U, lruIdx_r(0), lineToReplace_r(0))))
  val writeSet = Mux(WawHzd, writeSet_r(0), wsetIdx_r(0))
  writeSet_r(0) := writeSet
  writeWay_r(0) := writeWay

  io.data_entry :=  Mux(rvalid_r(0),readData,invalidData)
  writePort.way := writeWay
  writePort.set := writeSet
  when(writePort.enable) {
    for (i <- 0 until numWays) {
      when(writePort.way === i.U) {
        dataArray(i).write(writePort.set, writePort.data) 
        tagArray(writePort.way)(writePort.set) := wtag_r(0)
        validArray(writePort.way)(writePort.set) := true.B
      }
    }
  }

  when(wvalid_r(0) && (!WawHzd)){
      for (way <- 0 until numWays) {
      when(way.U === writeWay) {
        LruCounters(way)(writePort.set) := 0.U  // 设为最近使用
      }.otherwise {
        LruCounters(way)(writePort.set) := Mux(LruCounters(way)(writePort.set) === (numWays - 1).U,
                                          LruCounters(way)(writePort.set), // 已经是最大值，不变
                                          LruCounters(way)(writePort.set) + 1.U) // 其他+1
      }
    }
  } 
  
}


class PCDeltaTable(val keyWidth: Int, val numSets: Int, val numWays: Int, 
val DelDataWidth: Int, val tagWidth: Int, val DelTcWidth: Int, val DelNum: Int, 
val HisNum: Int, val DelWidth: Int, val DelLcWidth: Int, val PCConfMax: Int,val PageConfMax: Int)(implicit p: Parameters) extends BoomModule {
  val io = IO(new Bundle {
    val rkey = Input(UInt(keyWidth.W))
    val wkey = Input(UInt(keyWidth.W))
    val offsetIn = Input(new DeltaEleHis)
    val rvalid = Input(Bool())
    val wvalid = Input(Bool())
    val offsetsOut = Output(new DeltaData) 
    val offsetsNumOut = Output(UInt((log2Ceil(p(PrefDelCfg).DelNum) + 1).W))
  })
  val invalidData = Wire(new DeltaData()(p))
  invalidData := 0.U.asTypeOf(new DeltaData()(p))  // 或者根据实际需要初始化
  
  
  //reset信号，存储在BOOMMOdule中，一般使用when(reset.asBool)来判断
  //*************************存储逻辑*******************************//
  // 定义缓存结构以及初始化
  val dataArray = Seq.fill(numWays)(SyncReadMem(numSets, UInt(DelDataWidth.W)))
  val tagArray   = RegInit(VecInit(Seq.fill(numWays)(VecInit(Seq.fill(numSets)(0.U(tagWidth.W))))))
  val validArray = RegInit(VecInit(Seq.fill(numWays)(VecInit(Seq.fill(numSets)(false.B)))))
  val LruCounters = RegInit(VecInit(Seq.fill(numWays)(VecInit(Seq.fill(numSets)(0.U(log2Ceil(numWays).W))))))
  //初始化
  // when(reset.asBool){
  //   for (way <- 0 until numWays) {
  //     for (set <- 0 until numSets) {
  //       dataArray(way).write(set.U, ((new PCHistoryData()).default).asUInt()) 
  //     }
  //   }
  // }
  // 读写端口
  val readPort = Wire(new PTReadPort(numSets, numWays, DelDataWidth))
  val writePort = Wire(new PTWritePort(numSets, numWays, DelDataWidth))

  // 获取Idx和tag:stage 0
  val rsetIdx = io.rkey(log2Up(numSets)-1, 0)
  val wsetIdx = io.wkey(log2Up(numSets)-1, 0)
  readPort.set := Mux(io.rvalid, rsetIdx, Mux(io.wvalid, wsetIdx, 0.U))
  writePort.set := wsetIdx
  val rtag = io.rkey(tagWidth + log2Up(numSets) - 1, log2Up(numSets))
  val wtag = io.wkey(tagWidth + log2Up(numSets) - 1, log2Up(numSets))

  //寄存器组
   //寄存器组合，从stage0开始， data_r从 stage1开始
  val rvalid_r = RegInit(VecInit(Seq.fill(1)(false.B))) // 4 个 8 位宽的 `UInt`，初始值为 0
  val wvalid_r = RegInit(VecInit(Seq.fill(2)(false.B))) // 4 个 8 位宽的 `UInt`，初始值为 0
  val rkey_r = RegInit(VecInit(Seq.fill(2)(0.U(keyWidth.W))))
  val wkey_r = RegInit(VecInit(Seq.fill(2)(0.U(keyWidth.W))))
  val offsets_r = RegInit(VecInit(Seq.fill(1)(0.U(DelDataWidth.W))))
  val rtag_r = RegInit(VecInit(Seq.fill(1)(0.U(tagWidth.W))))
  val wtag_r = RegInit(VecInit(Seq.fill(1)(0.U(tagWidth.W))))
  val wdata_r = RegInit(VecInit(Seq.fill(1)({
    val d = Wire(new DeltaData()(p))
    d.deltas.zipWithIndex.foreach { case (ele, idx) =>
      ele.delta := 0.S(p(PrefDelCfg).DelWidth.W)
      ele.localCnt := 0.U(p(PrefDelCfg).DelLcWidth.W)
    }
    d.totalCnt := 0.U(log2Ceil(p(PrefDelCfg).DelTcWidth).W)
    d
  })))
  val writeSet_r = RegInit(VecInit(Seq.fill(1)(0.U(log2Ceil(numSets).W))))
  val writeWay_r = RegInit(VecInit(Seq.fill(1)(0.U(log2Ceil(numWays).W))))
  val rsetIdx_r = RegInit(VecInit(Seq.fill(1)(0.U((log2Ceil(numSets)).W))))
  val wsetIdx_r = RegInit(VecInit(Seq.fill(1)(0.U((log2Ceil(numSets)).W))))
  val rhit_r = RegInit(VecInit(Seq.fill(1)(false.B)))
  val whit_r = RegInit(VecInit(Seq.fill(1)(false.B)))
  val rhitWay_r = RegInit(VecInit(Seq.fill(1)(0.U((log2Ceil(numWays)).W))))
  val whitWay_r = RegInit(VecInit(Seq.fill(1)(0.U((log2Ceil(numWays)).W))))
  val lineToReplace_r = RegInit(VecInit(Seq.fill(1)(0.U((log2Ceil(numWays) + 1).W))))
  val lruIdx_r = RegInit(VecInit(Seq.fill(1)(0.U((log2Ceil(numWays)).W))))
  val RawHzd_r = RegInit(false.B)
  
  
  // 检查是否命:stage 1
  // 这里，在way和index更新后的第二个周期，validArray()()和tagArray()()才会更新相应的值
  // 第一组信号：是否命中，命中的话，数据是多少
  val rwayHits = VecInit((0 until numWays).map { way =>
     validArray(way)(rsetIdx) && (tagArray(way)(rsetIdx) === rtag) && io.rvalid})
  val rhit = rwayHits.reduce(_ || _)
  val wwayHits = VecInit((0 until numWays).map { way =>
     validArray(way)(wsetIdx) && (tagArray(way)(wsetIdx) === wtag) && io.wvalid})
  val whit = wwayHits.reduce(_ || _)
  val rhitWay = Mux(rhit, PriorityEncoder(rwayHits), numWays.asUInt) // 获取命中的way的索引
  val whitWay = Mux(whit, PriorityEncoder(wwayHits), numWays.asUInt) // 获取命中的way的索引
  // 第二组信号(for write):是否存在无效的路,以及LRU的数
  val invalidWay = VecInit((0 until numWays).map { way => 
    !(validArray(way)(wsetIdx))})
  val invalidWayExt = invalidWay.reduce(_ || _)
  val lineToReplace = Mux(invalidWayExt, PriorityEncoder(invalidWay), numWays.asUInt)
  val wayLruPairs = VecInit((0 until numWays).map(way => {
    val pair = Wire(Vec(2, UInt(log2Ceil(numWays).W)))
      pair(0) := LruCounters(way)(wsetIdx)  // LRU 计数
      pair(1) := way.U                      // Way 索引
      pair
    }))
  val lruIdx = wayLruPairs.reduce((a, b) => Mux(a(0) > b(0), a, b))(1)
  // 第三组信号，读出的数据，以及相应的替换的历史
  val RawHzd = ((io.rkey === wkey_r(0)) && wvalid_r(0) && io.rvalid )  
  RawHzd_r := RawHzd
  val WawHzd = (wkey_r(1)===wkey_r(0)) && wvalid_r(0) && wvalid_r(1)

  readPort.enable := Mux(RawHzd, false.B, (io.rvalid || io.wvalid))
  for (way <- 0 until numWays) {
    readPort.data(way) := dataArray(way).read(readPort.set, readPort.enable)}

  // stage 1
  val readData = Mux((RawHzd_r || WawHzd), wdata_r(0),
   Mux(rhit_r(0) , readPort.data(rhitWay_r(0)).asTypeOf(new DeltaData()(p)), 
   Mux(whit_r(0), readPort.data(whitWay_r(0)).asTypeOf(new DeltaData()(p)), invalidData))) 
  val DeltaEle = readData.deltas
  val zeroMas = VecInit(DeltaEle.map(_.localCnt === 0.U))
  val zeroIs = zeroMas.reduce(_ || _)
  val zeroIdx = Mux(zeroIs, PriorityEncoder(zeroMas), DelNum.U)


  
  val wdata_mid = readData
  val wdata = Wire(new DeltaData()(p))
  wdata      := readData
  
    //find the deltas in dt if euqals the deltas in Offsetin
  val foundForEntDel = Wire(Vec(DelNum, Bool()))
  for (i <- 0 until DelNum) {
    foundForEntDel(i) := false.B
      for (j <- 0 until HisNum) {
        when(j.U < io.offsetIn.nums){
          when(io.offsetIn.deltas(j) === wdata_mid.deltas(i).delta ) {
            wdata.deltas(i).localCnt  := wdata_mid.deltas(i).localCnt + 1.U
            foundForEntDel(i) := true.B
        }
      }
    }
  }

  val indexedVec = VecInit(DeltaEle.zipWithIndex.map {  case (value, idx) => Cat(Mux(foundForEntDel(idx) === true.B, value.localCnt + 1.U, value.localCnt), idx.U(log2Ceil(p(PrefDelCfg).DelNum).W)) }) 
  val leftIdxCat =  log2Ceil(p(PrefDelCfg).DelNum) + log2Ceil(p(PrefDelCfg).DelLcWidth) - 1
  val rightIdxCat =  log2Ceil(p(PrefDelCfg).DelNum) 
  val minCat = indexedVec.reduceTree { (a, b) => Mux(a(leftIdxCat, rightIdxCat) < b(leftIdxCat, rightIdxCat) , a, b)}
  val DelminVal = minCat(leftIdxCat, rightIdxCat)
  val Delmin = minCat(rightIdxCat - 1, 0)
  val isDelmin = (DeltaEle(Delmin).localCnt =/= 0.U)
 
  //find the deltas in Offsetin if euqals the deltas in dt
  val foundForOffset = Wire(Vec(HisNum, Bool()))
  for (k <- 0 until HisNum) {
    foundForOffset(k) := false.B
    when(k.U < io.offsetIn.nums ){
      for (l <- 0 until DelNum) {
        when(io.offsetIn.deltas(k) === wdata_mid.deltas(l).delta) {
          foundForOffset(k)  := true.B
        }
      }
    }
  }
  val prefixSum = Wire(Vec(HisNum, UInt((log2Ceil(HisNum)+1).W))) // 多一位方便计算
  prefixSum(0) := Mux(foundForOffset(0) === false.B, 1.U, 0.U)
  for (m <- 0 until (HisNum - 1)) {
    prefixSum(m+1) := prefixSum(m) + Mux(foundForOffset(m+1) === false.B, 1.U, 0.U)
  }

  when((prefixSum(io.offsetIn.nums - 1.U) =/= 0.U) && (zeroIdx === (p(PrefDelCfg).DelNum).U) && (isDelmin)){
    wdata.deltas(Delmin).delta := Mux(io.offsetIn.nums =/= 0.U, io.offsetIn.deltas(0), wdata_mid.deltas(Delmin).delta)
    wdata.deltas(Delmin).localCnt := Mux(io.offsetIn.nums =/= 0.U, 1.U, wdata_mid.deltas(Delmin).localCnt)
  } .otherwise{
    for (n <- 0 until HisNum ) {
      when((foundForOffset(n) === false.B) && (n.U < io.offsetIn.nums)){
        val Idx = zeroIdx + prefixSum(n) - 1.U
        wdata.deltas(Mux(Idx < DelNum.U, Idx, DelNum.U - 1.U)).delta := io.offsetIn.deltas(n)
        wdata.deltas(Mux(Idx < DelNum.U, Idx, DelNum.U - 1.U)).localCnt := 1.U
      } 
    } 
  }
  val NewDelNum = Mux(io.offsetIn.nums === 0.U, 0.U, prefixSum(io.offsetIn.nums - 1.U))
  io.offsetsNumOut := (zeroIdx + NewDelNum) % ((p(PrefDelCfg).DelNum).U + 1.U)
  
  wdata.totalCnt := Mux(io.offsetIn.nums =/= 0.U, wdata_mid.totalCnt + 1.U, wdata_mid.totalCnt)

  //相关存储操作：
  //寄存器组合，从setIdx从stage0开始，其他从stage1开始
  rsetIdx_r(0) := rsetIdx
  wsetIdx_r(0) := wsetIdx
  rhit_r(0) := rhit
  whit_r(0) := whit
  rhitWay_r(0) := rhitWay
  whitWay_r(0) := whitWay
  lineToReplace_r(0) := lineToReplace
  lruIdx_r(0) := lruIdx
  for(i <- 0 until 0){
    rsetIdx_r(i+1) := rsetIdx_r(i)
    wsetIdx_r(i+1) := wsetIdx_r(i)
    lineToReplace_r(i+1) := lineToReplace_r(i)
    lruIdx_r(i+1) := lruIdx_r(i)
    rhit_r(i+1) := rhit_r(i)
    whit_r(i+1) := whit_r(i)
    rhitWay_r(i+1) := rhitWay_r(i)
    whitWay_r(i+1) := whitWay_r(i)
  }

  //相关控制操作
  rvalid_r(0) := io.rvalid
  wvalid_r(0) := io.wvalid
  rkey_r(0) := io.rkey
  wkey_r(0) := io.wkey
  wdata_r(0) := wdata
  rtag_r(0) := rtag
  wtag_r(0) := wtag

  for(i <- 0 until 0){
    rvalid_r(i+1) := rvalid_r(i)
    rtag_r(i + 1) := rtag_r(i)
    wtag_r(i + 1) := wtag_r(i)
  }
  for(i <- 0 until 1){
    wvalid_r(i +1 ) := wvalid_r(i)
    rkey_r(i + 1) := rkey_r(i)
    wkey_r(i + 1) := wkey_r(i)
  }
  for(i <- 0 until 0){
    wdata_r(i + 1) := wdata_r(i)
    writeSet_r(i + 1) := writeSet_r(i)
    writeWay_r(i + 1) := writeWay_r(i)
  }

  
  // stage 1: 取出数据, 判断是否有相同地址等
  writePort.enable := Mux(wvalid_r(0), true.B, false.B )
  writePort.data := wdata.asUInt
  val writeWay = Mux(WawHzd, writeWay_r(0), Mux(whit_r(0), whitWay_r(0).asUInt,Mux(lineToReplace_r(0) === numWays.U, lruIdx_r(0), lineToReplace_r(0))))
  val writeSet = Mux(WawHzd, writeSet_r(0), wsetIdx_r(0))
  writeSet_r(0) := writeSet
  writeWay_r(0) := writeWay

  io.offsetsOut := Mux(rvalid_r(0),Mux(wvalid_r(0) && (wkey_r(0) === rkey_r(0)), wdata,readData),invalidData)
  writePort.way := writeWay 
  writePort.set := writeSet
  when(writePort.enable) {
    for (i <- 0 until numWays) {
      when(writePort.way === i.U) {
        dataArray(i).write(writePort.set, writePort.data) 
        tagArray(writePort.way)(writePort.set) := wtag_r(0)
        validArray(writePort.way)(writePort.set) := true.B
      }
    }
  }

  when(wvalid_r(0) && (!WawHzd)){
      for (way <- 0 until numWays) {
      when(way.U === writeWay) {
        LruCounters(way)(writePort.set) := 0.U  // 设为最近使用
      }.otherwise {
        LruCounters(way)(writePort.set) := Mux(LruCounters(way)(writePort.set) === (numWays - 1).U,
                                          LruCounters(way)(writePort.set), // 已经是最大值，不变
                                          LruCounters(way)(writePort.set) + 1.U) // 其他+1
      }
    }
  } 
  
}






