
package boom.lsu

import chisel3._
import chisel3.util.{log2Ceil}
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


//using: val myCacheLine = Wire(new PrefCacheLine(16, UInt(32.W)))
class DeltaData(val TcWidth: Int, val DelNum: Int, val DelWidth: Int, val LcWidth : Int) extends Bundle {
  val Total_cnt = UInt(TcWidth.W)
  val Deltas = Vec(DelNum, new Bundle {
    val address = UInt(DelWidth.W)  // 地址位宽
    val timestamp = UInt(LcWidth.W)  // 时间戳位宽
  }
  )
}



class PCHistoryTable(val keyWidth: Int, val numSets: Int, val numWays: Int, 
val HisDataWidth: Int, val tagWidth: Int, val HisNum: Int, val HisAdrWidth: Int, 
val HisTsWidth: Int)(implicit p: Parameters) extends BoomModule {
  val io = IO(new Bundle {
    val key = Input(UInt(keyWidth.W))
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
  val setIdx = io.key(log2Up(numSets)-1, 0)
  readPort.set := setIdx  // 获取缓存组索引
  val tag = io.key(tagWidth + log2Up(numSets) - 1, log2Up(numSets))

  //寄存器组
   //寄存器组合，从stage0开始， data_r从 stage1开始
  val rvalid_r = RegInit(VecInit(Seq.fill(3)(false.B))) // 4 个 8 位宽的 `UInt`，初始值为 0
  val wvalid_r = RegInit(VecInit(Seq.fill(3)(false.B))) // 4 个 8 位宽的 `UInt`，初始值为 0
  val key_r = RegInit(VecInit(Seq.fill(3)(0.U(keyWidth.W))))
  val addr_r = RegInit(VecInit(Seq.fill(3)(0.U(HisAdrWidth.W))))
  val tag_r = RegInit(VecInit(Seq.fill(3)(0.U(tagWidth.W))))
  val ts_r = RegInit(VecInit(Seq.fill(3)(0.U(HisTsWidth.W))))
  val wdata_r = RegInit(VecInit(Seq.fill(2)({
    val pchData = Wire(new PCHistoryData()(p))
    pchData.His.zipWithIndex.foreach { case (ele, idx) =>
      ele.address := 0.U(p(PCPrefHisCfg).HisAdrWidth.W)
      ele.timestamp := 0.U(p(PCPrefHisCfg).HisTsWidth.W)
    }
    pchData.head := 0.U(log2Ceil(p(PCPrefHisCfg).HisNum).W)
    pchData
  })))
  val writeSet_r = RegInit(VecInit(Seq.fill(2)(0.U(log2Ceil(numSets).W))))
  val writeWay_r = RegInit(VecInit(Seq.fill(2)(0.U(log2Ceil(numWays).W))))
  val setIdx_r = RegInit(VecInit(Seq.fill(3)(0.U((log2Ceil(numSets)).W))))
  val hit_r = RegInit(VecInit(Seq.fill(3)(false.B)))
  val hitWay_r = RegInit(VecInit(Seq.fill(3)(0.U((log2Ceil(numWays)).W))))
  val lineToReplace_r = RegInit(VecInit(Seq.fill(3)(0.U((log2Ceil(numWays) + 1).W))))
  val lruIdx_r = RegInit(VecInit(Seq.fill(3)(0.U((log2Ceil(numWays)).W))))
  val RawHzd_r = RegInit(false.B)
  
  
  // 检查是否命:stage 1
  // 这里，在way和index更新后的第二个周期，validArray()()和tagArray()()才会更新相应的值
  // 第一组信号：是否命中，命中的话，数据是多少
  val wayHits = VecInit((0 until numWays).map { way =>
     validArray(way)(setIdx) && (tagArray(way)(setIdx) === tag)})
  val hit = wayHits.reduce(_ || _)
  val hitWay = Mux(hit, PriorityEncoder(wayHits), numWays.asUInt) // 获取命中的way的索引
  // 第二组信号(for write):是否存在无效的路,以及LRU的数
  val invalidWay = VecInit((0 until numWays).map { way => 
    !(validArray(way)(setIdx))})
  val invalidWayExt = invalidWay.reduce(_ || _)
  val lineToReplace = Mux(invalidWayExt, PriorityEncoder(invalidWay), numWays.asUInt)
  val wayLruPairs = VecInit((0 until numWays).map(way => {
    val pair = Wire(Vec(2, UInt(log2Ceil(numWays).W)))
      pair(0) := LruCounters(way)(setIdx)  // LRU 计数
      pair(1) := way.U                      // Way 索引
      pair
    }))
  val lruIdx = wayLruPairs.reduce((a, b) => Mux(a(0) > b(0), a, b))(1)
  // 第三组信号，读出的数据，以及相应的替换的历史
  val RawHzd = ((io.key === key_r(0)) && wvalid_r(0) && io.rvalid )  
  RawHzd_r := RawHzd

  readPort.enable := Mux(RawHzd, false.B, (io.rvalid || io.wvalid))
  for (way <- 0 until numWays) {
    readPort.data(way) := dataArray(way).read(readPort.set, readPort.enable)}

  // stage 1
  val readData = Mux(RawHzd_r, wdata_r(0), Mux(hit_r(0), readPort.data(hitWay_r(0)).asTypeOf(new PCHistoryData()(p)), invalidData))  
  val hisEle_safe = readData.His
  val HisVec_safe = hisEle_safe.map(entry => entry.address === addr_r(0)) // 找到第一个匹配的位置
  val HisMatchIdx_safe = Mux(HisVec_safe.reduce(_ || _), PriorityEncoder(HisVec_safe), HisNum.U) // 找不到时返回 HisNum.U
  val updated_data_w_safe = readData
  val HisLruIdx_safe = readData.head
  val hisEle_hzd = wdata_r(0).His
  val HisVec_hzd = hisEle_hzd.map(entry => entry.address === addr_r(0)) // 找到第一个匹配的位置
  val HisMatchIdx_hzd = Mux(HisVec_hzd.reduce(_ || _), PriorityEncoder(HisVec_hzd), HisNum.U) // 找不到时返回 HisNum.U
  val updated_data_w_hzd = wdata_r(0)
  val HisLruIdx_hzd =  wdata_r(0).head 
  val WawHzd = (key_r(1)===key_r(0)) && wvalid_r(0) && wvalid_r(1)
  val wdata = Wire(new PCHistoryData()(p))
  wdata := Mux(WawHzd, wdata_r(0),readData)
  val wdataHead = Mux(WawHzd, wdata_r(0).head, readData.head)
  wdata.His(wdataHead).address := addr_r(0)
  wdata.His(wdataHead).timestamp := ts_r(0)
  wdata.head := (wdataHead + 1.U) % HisNum.U
  
  

  //相关存储操作：
  //寄存器组合，从setIdx从stage0开始，其他从stage1开始
  setIdx_r(0) := setIdx
  hit_r(0) := hit
  hitWay_r(0) := hitWay
  lineToReplace_r(0) := lineToReplace
  lruIdx_r(0) := lruIdx
  for(i <- 0 until 2){
    setIdx_r(i+1) := setIdx_r(i)
    lineToReplace_r(i+1) := lineToReplace_r(i)
    lruIdx_r(i+1) := lruIdx_r(i)
    hit_r(i+1) := hit_r(i)
    hitWay_r(i+1) := hitWay_r(i)
  }

  //相关控制操作
  rvalid_r(0) := io.rvalid
  wvalid_r(0) := io.wvalid
  key_r(0) := io.key
  addr_r(0) := io.addr
  ts_r(0) := io.timestamp
  wdata_r(0) := wdata
  tag_r(0) := tag

  for(i <- 0 until 2){
    rvalid_r(i+1) := rvalid_r(i)
    wvalid_r(i +1 ) := wvalid_r(i)
    key_r(i + 1) := key_r(i)
    addr_r(i + 1) := addr_r(i)
    ts_r(i + 1) := ts_r(i)
    tag_r(i + 1) := tag_r(i)
  }
  for(i <- 0 until 1){
    wdata_r(i + 1) := wdata_r(i)
    writeSet_r(i + 1) := writeSet_r(i)
    writeWay_r(i + 1) := writeWay_r(i)
  }

  
  // stage 1: 取出数据, 判断是否有相同地址等
  writePort.enable := Mux(wvalid_r(0), Mux((WawHzd && HisMatchIdx_hzd === HisNum.U) || (HisMatchIdx_safe === HisNum.U), true.B, false.B ), false.B)
  writePort.data := wdata.asUInt
  val writeWay = Mux(WawHzd, writeWay_r(0), Mux(hit_r(0), hitWay_r(0).asUInt,Mux(lineToReplace_r(0) === numWays.U, lruIdx_r(0), lineToReplace_r(0))))
  val writeSet = Mux(WawHzd, writeSet_r(0), setIdx_r(0))
  writeSet_r(0) := writeSet
  writeWay_r(0) := writeWay

  io.data_entry := Mux(rvalid_r(0), readData , invalidData)
  writePort.way := writeWay
  writePort.set := writeSet
  when(writePort.enable) {
    for (i <- 0 until numWays) {
      when(writePort.way === i.U) {
        dataArray(i).write(writePort.set, writePort.data) 
        tagArray(writePort.way)(writePort.set) := tag_r(0)
        validArray(writePort.way)(writePort.set) := true.B
      }
    }
  }

  // 更新tag

  //更新lru
  when(rvalid_r(0) && (!RawHzd_r) && hit){
      for (way <- 0 until numWays) {
      when(way.U === hitWay) {
        LruCounters(way)(setIdx_r(0)) := 0.U  // 设为最近使用
      }.otherwise {
        LruCounters(way)(setIdx_r(0)) := Mux(LruCounters(way)(setIdx_r(0)) === (numWays - 1).U,
                                          LruCounters(way)(setIdx_r(0)), // 已经是最大值，不变
                                          LruCounters(way)(setIdx_r(0)) + 1.U) // 其他+1
      }
    }
  } 
  when(wvalid_r(0) && (!WawHzd)){
      for (way <- 0 until numWays) {
      when(way.U === writeWay) {
        LruCounters(way)(setIdx_r(0)) := 0.U  // 设为最近使用
      }.otherwise {
        LruCounters(way)(setIdx_r(0)) := Mux(LruCounters(way)(setIdx_r(0)) === (numWays - 1).U,
                                          LruCounters(way)(setIdx_r(0)), // 已经是最大值，不变
                                          LruCounters(way)(setIdx_r(0)) + 1.U) // 其他+1
      }
    }
  } 
  
}






