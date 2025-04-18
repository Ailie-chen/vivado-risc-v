//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

package boom.common

import chisel3._
import chisel3.util.{log2Up}
import chisel3.util.{log2Ceil}

import freechips.rocketchip.config.{Parameters, Config, Field}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.devices.tilelink.{BootROMParams}
import freechips.rocketchip.diplomacy.{SynchronousCrossing, AsynchronousCrossing, RationalCrossing}
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._

import boom.ifu._
import boom.exu._
import boom.lsu._


//add by ailie
class PrefCacheBaseConfig(
  val numSets: Int,  
  val numWays: Int, 
  val tagWidth: Int   
){
  val keyWidth = tagWidth + log2Ceil(numSets)
}

class PrefHisTableConfig(
  val HisNum: Int,
  val HisAdrWidth: Int,
  val HisTsWidth: Int,
  override val numSets: Int = 8,
  override val numWays: Int = 6,
  override val tagWidth: Int = 12
) extends PrefCacheBaseConfig( 
  numSets = numSets,            
  numWays = numWays,            
  tagWidth = tagWidth  
) {
  val HisDataWidth = HisNum * (HisAdrWidth + HisTsWidth)
}

class PrefDelTableConfig(
  val DelTcWidth: Int,
  val DelNum: Int,
  val DelWidth: Int,
  val DelLcWidth: Int,
  val PCConfMax: Int,
  val PageConfMax: Int,
  override val numSets: Int = 8,
  override val numWays: Int = 6,
  override val tagWidth: Int = 12
) extends PrefCacheBaseConfig( 
  numSets = numSets,            
  numWays = numWays,            
  tagWidth = tagWidth  
) {
  val DelDataWidth = DelTcWidth + DelNum * (DelWidth + DelLcWidth)
}

class CacheLatConfig(
  val LatWidth: Int,
  val TsCntWidth: Int,
  val PrefThresh: Int // the value has been multiplied by 256
){}

class PrefQConfig(
  val CmdNum        : Int,
  val PrefNum       : Int,
  val DeltasRegNum  : Int
){}

class HermesConfig (
  val featuresNum: Int,
  val FWidth: Int,
  val WWidth: Int,
  val BiasWidth: Int  
){}

case object HermesCfg extends Field[HermesConfig]

case object HyperionDefKey extends Field[Boolean]
case object PCPrefHisCfg extends Field[PrefHisTableConfig]
case object PagePrefHisCfg extends Field[PrefHisTableConfig]
case object PrefDelCfg extends Field[PrefDelTableConfig]
case object L1DPrefCfg extends Field[CacheLatConfig]
case object QCfg extends Field[PrefQConfig]

// ---------------------
// BOOM Config Fragments
// ---------------------

class WithBoomCommitLogPrintf extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: BoomTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(core = tp.tileParams.core.copy(
      enableCommitLogPrintf = true
    )))
    case other => other
  }
})


class WithBoomBranchPrintf extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: BoomTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(core = tp.tileParams.core.copy(
      enableBranchPrintf = true
    )))
    case other => other
  }
})

class WithNBoomPerfCounters(n: Int) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: BoomTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(core = tp.tileParams.core.copy(
      nPerfCounters = n
    )))
    case other => other
  }
})


class WithSynchronousBoomTiles extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: BoomTileAttachParams => tp.copy(crossingParams = tp.crossingParams.copy(
      crossingType = SynchronousCrossing()
    ))
    case other => other
  }
})

class WithAsynchronousBoomTiles extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: BoomTileAttachParams => tp.copy(crossingParams = tp.crossingParams.copy(
      crossingType = AsynchronousCrossing()
    ))
    case other => other
  }
})

class WithRationalBoomTiles extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: BoomTileAttachParams => tp.copy(crossingParams = tp.crossingParams.copy(
      crossingType = RationalCrossing()
    ))
    case other => other
  }
})



/**
 * 1-wide BOOM.
 */
class WithNSmallBooms(n: Int = 1, overrideIdOffset: Option[Int] = None) extends Config(
  new WithTAGELBPD ++ // Default to TAGE-L BPD
  new Config((site, here, up) => {

    //add by ailie: control of hyperion
    case HyperionDefKey => true // 开启 HyperionDefKey
    //add by ailie: cfg of tables in Hypeiron
    case PCPrefHisCfg => new PrefHisTableConfig( 
      numSets = 2,
      numWays = 4,
      tagWidth = 10,
      HisNum = 4,            
      HisAdrWidth = 24,   
      HisTsWidth = 16       
    )
    case PagePrefHisCfg => new PrefHisTableConfig(      
      HisNum = 8,          
      HisAdrWidth = 24,   
      HisTsWidth = 16     
    )
    case PrefDelCfg => new PrefDelTableConfig(
      numSets = 2,
      numWays = 4,
      tagWidth = 10,             
      DelTcWidth = 4,     
      DelNum = 4,         
      DelWidth = 13,     
      DelLcWidth = 4,
      PCConfMax = 15,
      PageConfMax = 15    
    )

    case L1DPrefCfg => new CacheLatConfig(             
      LatWidth = 12,
      TsCntWidth = 24,
      PrefThresh = 153
    )

    case QCfg => new PrefQConfig(

      CmdNum = 12,
      PrefNum = 12,
      DeltasRegNum = 3
    )

    case HermesCfg => new HermesConfig(
      featuresNum = 2,
      FWidth = 4,
      WWidth = 5,
      BiasWidth = 5
    )

    case TilesLocated(InSubsystem) => {
      val prev = up(TilesLocated(InSubsystem), site)
      val idOffset = overrideIdOffset.getOrElse(prev.size)
      (0 until n).map { i =>
        BoomTileAttachParams(
          tileParams = BoomTileParams(
            core = BoomCoreParams(
              fetchWidth = 4,
              decodeWidth = 1,
              numRobEntries = 32,
              issueParams = Seq(
                IssueParams(issueWidth=1, numEntries=8, iqType=IQT_MEM.litValue, dispatchWidth=1),
                IssueParams(issueWidth=1, numEntries=8, iqType=IQT_INT.litValue, dispatchWidth=1),
                IssueParams(issueWidth=1, numEntries=8, iqType=IQT_FP.litValue , dispatchWidth=1)),
              numIntPhysRegisters = 52,
              numFpPhysRegisters = 48,
              numLdqEntries = 8,
              numStqEntries = 8,
              maxBrCount = 8,
              numFetchBufferEntries = 8,
              ftq = FtqParameters(nEntries=16),
              nPerfCounters = 2,
              enablePrefetching = true,
              fpu = Some(freechips.rocketchip.tile.FPUParams(sfmaLatency=4, dfmaLatency=4, divSqrt=true))
            ),
            dcache = Some(
              DCacheParams(rowBits = site(SystemBusKey).beatBits, nSets=64, nWays=4, nMSHRs=2, nTLBWays=8)
            ),
            icache = Some(
              ICacheParams(rowBits = site(SystemBusKey).beatBits, nSets=64, nWays=4, fetchBytes=2*4)
            ),
            hartId = i + idOffset
          ),
          crossingParams = RocketCrossingParams()
        )
      } ++ prev
    }
    case SystemBusKey => up(SystemBusKey, site).copy(beatBytes = 8)
    case XLen => 64
    
  })
)

/**
 * 2-wide BOOM.
 */
class WithNMediumBooms(n: Int = 1, overrideIdOffset: Option[Int] = None) extends Config(
  new WithTAGELBPD ++ // Default to TAGE-L BPD
  new Config((site, here, up) => {
    //add by ailie: control of hyperion
    case HyperionDefKey => true // 开启 HyperionDefKey
    //add by ailie: cfg of tables in Hypeiron
    case PCPrefHisCfg => new PrefHisTableConfig( 
      numSets = 2,
      numWays = 2,
      tagWidth = 10,
      HisNum = 2,            
      HisAdrWidth = 24,   
      HisTsWidth = 16       
    )
    case PagePrefHisCfg => new PrefHisTableConfig(      
      HisNum = 8,          
      HisAdrWidth = 24,   
      HisTsWidth = 16     
    )
    case PrefDelCfg => new PrefDelTableConfig(
      numSets = 2,
      numWays = 2,
      tagWidth = 10,             
      DelTcWidth = 4,     
      DelNum = 2,         
      DelWidth = 13,     
      DelLcWidth = 4,
      PCConfMax = 15,
      PageConfMax = 15    
    )

    case L1DPrefCfg => new CacheLatConfig(             
      LatWidth = 12,
      TsCntWidth = 24,
      PrefThresh = 153
    )

    case QCfg => new PrefQConfig(

      CmdNum = 12,
      PrefNum = 12,
      DeltasRegNum = 2
    )
    case HermesCfg => new HermesConfig(
      featuresNum = 2,
      FWidth = 4,
      WWidth = 5,
      BiasWidth = 5
    )
    case TilesLocated(InSubsystem) => {
      val prev = up(TilesLocated(InSubsystem), site)
      val idOffset = overrideIdOffset.getOrElse(prev.size)
      (0 until n).map { i =>
        BoomTileAttachParams(
          tileParams = BoomTileParams(
            core = BoomCoreParams(
              fetchWidth = 4,
              decodeWidth = 2,
              numRobEntries = 64,
              issueParams = Seq(
                IssueParams(issueWidth=1, numEntries=12, iqType=IQT_MEM.litValue, dispatchWidth=2),
                IssueParams(issueWidth=2, numEntries=20, iqType=IQT_INT.litValue, dispatchWidth=2),
                IssueParams(issueWidth=1, numEntries=16, iqType=IQT_FP.litValue , dispatchWidth=2)),
              numIntPhysRegisters = 80,
              numFpPhysRegisters = 64,
              numLdqEntries = 16,
              numStqEntries = 16,
              maxBrCount = 12,
              numFetchBufferEntries = 16,
              enablePrefetching = true,
              ftq = FtqParameters(nEntries=32),
              nPerfCounters = 6,
              fpu = Some(freechips.rocketchip.tile.FPUParams(sfmaLatency=4, dfmaLatency=4, divSqrt=true))
            ),
            dcache = Some(
              DCacheParams(rowBits = site(SystemBusKey).beatBits, nSets=64, nWays=4, nMSHRs=2, nTLBWays=8)
            ),
            icache = Some(
              ICacheParams(rowBits = site(SystemBusKey).beatBits, nSets=64, nWays=4, fetchBytes=2*4)
            ),
            hartId = i + idOffset
          ),
          crossingParams = RocketCrossingParams()
        )
      } ++ prev
    }
    case SystemBusKey => up(SystemBusKey, site).copy(beatBytes = 8)
    case XLen => 64
  })
)
// DOC include start: LargeBoomConfig
/**
 * 3-wide BOOM. Try to match the Cortex-A15.
 */
class WithNLargeBooms(n: Int = 1, overrideIdOffset: Option[Int] = None) extends Config(
  new WithTAGELBPD ++ // Default to TAGE-L BPD
  new Config((site, here, up) => {
    case TilesLocated(InSubsystem) => {
      val prev = up(TilesLocated(InSubsystem), site)
      val idOffset = overrideIdOffset.getOrElse(prev.size)
      (0 until n).map { i =>
        BoomTileAttachParams(
          tileParams = BoomTileParams(
            core = BoomCoreParams(
              fetchWidth = 8,
              decodeWidth = 3,
              numRobEntries = 96,
              issueParams = Seq(
                IssueParams(issueWidth=1, numEntries=16, iqType=IQT_MEM.litValue, dispatchWidth=3),
                IssueParams(issueWidth=3, numEntries=32, iqType=IQT_INT.litValue, dispatchWidth=3),
                IssueParams(issueWidth=1, numEntries=24, iqType=IQT_FP.litValue , dispatchWidth=3)),
              numIntPhysRegisters = 100,
              numFpPhysRegisters = 96,
              numLdqEntries = 24,
              numStqEntries = 24,
              maxBrCount = 16,
              numFetchBufferEntries = 24,
              ftq = FtqParameters(nEntries=32),
              fpu = Some(freechips.rocketchip.tile.FPUParams(sfmaLatency=4, dfmaLatency=4, divSqrt=true))
            ),
            dcache = Some(
              DCacheParams(rowBits = site(SystemBusKey).beatBits, nSets=64, nWays=8, nMSHRs=4, nTLBWays=16)
            ),
            icache = Some(
              ICacheParams(rowBits = site(SystemBusKey).beatBits, nSets=64, nWays=8, fetchBytes=4*4)
            ),
            hartId = i + idOffset
          ),
          crossingParams = RocketCrossingParams()
        )
      } ++ prev
    }
    case SystemBusKey => up(SystemBusKey, site).copy(beatBytes = 16)
    case XLen => 64
  })
)
// DOC include end: LargeBoomConfig

/**
 * 4-wide BOOM.
 */
class WithNMegaBooms(n: Int = 1, overrideIdOffset: Option[Int] = None) extends Config(
  new WithTAGELBPD ++ // Default to TAGE-L BPD
  new Config((site, here, up) => {
    case TilesLocated(InSubsystem) => {
      val prev = up(TilesLocated(InSubsystem), site)
      val idOffset = overrideIdOffset.getOrElse(prev.size)
      (0 until n).map { i =>
        BoomTileAttachParams(
          tileParams = BoomTileParams(
            core = BoomCoreParams(
              fetchWidth = 8,
              decodeWidth = 4,
              numRobEntries = 128,
              issueParams = Seq(
                IssueParams(issueWidth=2, numEntries=24, iqType=IQT_MEM.litValue, dispatchWidth=4),
                IssueParams(issueWidth=4, numEntries=40, iqType=IQT_INT.litValue, dispatchWidth=4),
                IssueParams(issueWidth=2, numEntries=32, iqType=IQT_FP.litValue , dispatchWidth=4)),
              numIntPhysRegisters = 128,
              numFpPhysRegisters = 128,
              numLdqEntries = 32,
              numStqEntries = 32,
              maxBrCount = 20,
              numFetchBufferEntries = 32,
              enablePrefetching = true,
              ftq = FtqParameters(nEntries=40),
              fpu = Some(freechips.rocketchip.tile.FPUParams(sfmaLatency=4, dfmaLatency=4, divSqrt=true))
            ),
            dcache = Some(
              DCacheParams(rowBits = site(SystemBusKey).beatBits, nSets=64, nWays=8, nMSHRs=8, nTLBWays=32)
            ),
            icache = Some(
              ICacheParams(rowBits = site(SystemBusKey).beatBits, nSets=64, nWays=8, fetchBytes=4*4)
            ),
            hartId = i + idOffset
          ),
          crossingParams = RocketCrossingParams()
        )
      } ++ prev
    }
    case SystemBusKey => up(SystemBusKey, site).copy(beatBytes = 16)
    case XLen => 64
  })
)

/**
 * 5-wide BOOM.
  */
class WithNGigaBooms(n: Int = 1, overrideIdOffset: Option[Int] = None) extends Config(
  new WithTAGELBPD ++ // Default to TAGE-L BPD
  new Config((site, here, up) => {
    case TilesLocated(InSubsystem) => {
      val prev = up(TilesLocated(InSubsystem), site)
      val idOffset = overrideIdOffset.getOrElse(prev.size)
      (0 until n).map { i =>
        BoomTileAttachParams(
          tileParams = BoomTileParams(
            core = BoomCoreParams(
              fetchWidth = 8,
              decodeWidth = 5,
              numRobEntries = 130,
              issueParams = Seq(
                IssueParams(issueWidth=2, numEntries=24, iqType=IQT_MEM.litValue, dispatchWidth=5),
                IssueParams(issueWidth=5, numEntries=40, iqType=IQT_INT.litValue, dispatchWidth=5),
                IssueParams(issueWidth=2, numEntries=32, iqType=IQT_FP.litValue , dispatchWidth=5)),
              numIntPhysRegisters = 128,
              numFpPhysRegisters = 128,
              numLdqEntries = 32,
              numStqEntries = 32,
              maxBrCount = 20,
              numFetchBufferEntries = 32,
              enablePrefetching = true,
              numDCacheBanks = 1,
              ftq = FtqParameters(nEntries=40),
              fpu = Some(freechips.rocketchip.tile.FPUParams(sfmaLatency=4, dfmaLatency=4, divSqrt=true))
            ),
            dcache = Some(
              DCacheParams(rowBits = site(SystemBusKey).beatBits, nSets=64, nWays=8, nMSHRs=8, nTLBWays=32)
            ),
            icache = Some(
              ICacheParams(rowBits = site(SystemBusKey).beatBits, nSets=64, nWays=8, fetchBytes=4*4)
            ),
            hartId = i + idOffset
          ),
          crossingParams = RocketCrossingParams()
        )
      } ++ prev
    }
    case SystemBusKey => up(SystemBusKey, site).copy(beatBytes = 16)
    case XLen => 64
  })
)

/**
  * BOOM Configs for CS152 lab
  */
class WithNCS152BaselineBooms(n: Int = 1, overrideIdOffset: Option[Int] = None) extends Config(
  new WithTAGELBPD ++ // Default to TAGE-L BPD
  new Config((site, here, up) => {
    case TilesLocated(InSubsystem) => {
      val prev = up(TilesLocated(InSubsystem), site)
      val idOffset = overrideIdOffset.getOrElse(prev.size)
      (0 until n).map { i =>
        val coreWidth = 1                     // CS152: Change me (1 to 4)
        val memWidth = 1                      // CS152: Change me (1 or 2)
        BoomTileAttachParams(
          tileParams = BoomTileParams(
            core = BoomCoreParams(
              fetchWidth = 4,                   // CS152: Change me (4 or 8)
              numRobEntries = 4,                // CS152: Change me (2+)
              numIntPhysRegisters = 33,         // CS152: Change me (33+)
              numLdqEntries = 8,                // CS152: Change me (2+)
              numStqEntries = 8,                // CS152: Change me (2+)
              maxBrCount = 8,                   // CS152: Change me (2+)
              enableBranchPrediction = false,   // CS152: Change me
              numRasEntries = 0,                // CS152: Change me

              // DO NOT CHANGE BELOW
              enableBranchPrintf = true,
              decodeWidth = coreWidth,
              numFetchBufferEntries = coreWidth * 8,
              numDCacheBanks = memWidth,
              issueParams = Seq(
                IssueParams(issueWidth=memWidth,  numEntries=8,  iqType=IQT_MEM.litValue, dispatchWidth=coreWidth),
                IssueParams(issueWidth=coreWidth, numEntries=32, iqType=IQT_INT.litValue, dispatchWidth=coreWidth),
                IssueParams(issueWidth=1,         numEntries=4,  iqType=IQT_FP.litValue , dispatchWidth=coreWidth))
                // DO NOT CHANGE ABOVE
            ),
            dcache = Some(DCacheParams(
              rowBits=site(SystemBusKey).beatBytes*8,
              nSets=64, // CS152: Change me (must be pow2, 2-64)
              nWays=4,  // CS152: Change me (1-8)
              nMSHRs=2  // CS152: Change me (1+)
            )),
            hartId = i + idOffset
          ),
          crossingParams = RocketCrossingParams()
        )
      } ++ prev
    }
    case SystemBusKey => up(SystemBusKey, site).copy(beatBytes = 8)
    case XLen => 64
  })
)

class WithNCS152DefaultBooms(n: Int = 1, overrideIdOffset: Option[Int] = None) extends Config(
  new WithTAGELBPD ++ // Default to TAGE-L BPD
  new Config((site, here, up) => {
    case TilesLocated(InSubsystem) => {
      val prev = up(TilesLocated(InSubsystem), site)
      val idOffset = overrideIdOffset.getOrElse(prev.size)
      (0 until n).map { i =>
        val coreWidth = 3                     // CS152: Change me (1 to 4)
        val memWidth = 1                      // CS152: Change me (1 or 2)
        val nIssueSlots = 32                  // CS152: Change me (2+)
        BoomTileAttachParams(
          tileParams = BoomTileParams(
            core = BoomCoreParams(
              fetchWidth = 4,                   // CS152: Change me (4 or 8)
              numRobEntries = 96,               // CS152: Change me (2+)
              numIntPhysRegisters = 96,         // CS152: Change me (33+)
              numLdqEntries = 16,               // CS152: Change me (2+)
              numStqEntries = 16,               // CS152: Change me (2+)
              maxBrCount = 12,                  // CS152: Change me (2+)
              enableBranchPrediction = true,    // CS152: Change me
              numRasEntries = 16,               // CS152: Change me

              // DO NOT CHANGE BELOW
              enableBranchPrintf = true,
              decodeWidth = coreWidth,
              numFetchBufferEntries = coreWidth * 8,
              numDCacheBanks = memWidth,
              issueParams = Seq(
                IssueParams(issueWidth=memWidth,  numEntries=nIssueSlots, iqType=IQT_MEM.litValue, dispatchWidth=coreWidth),
                IssueParams(issueWidth=coreWidth, numEntries=nIssueSlots, iqType=IQT_INT.litValue, dispatchWidth=coreWidth),
                IssueParams(issueWidth=1,         numEntries=nIssueSlots, iqType=IQT_FP.litValue , dispatchWidth=coreWidth))
                // DO NOT CHANGE ABOVE
            ),
            dcache = Some(DCacheParams(
              rowBits=site(SystemBusKey).beatBytes*8,
              nSets=64, // CS152: Change me (must be pow2, 2-64)
              nWays=4,  // CS152: Change me (1-8)
              nMSHRs=2  // CS152: Change me (1+)
            )),
            hartId = i + idOffset
          ),
          crossingParams = RocketCrossingParams()
        )
      } ++ prev
    }
    case SystemBusKey => up(SystemBusKey, site).copy(beatBytes = 8)
    case XLen => 64
  })
)

/**
  *  Branch prediction configs below
  */

class WithTAGELBPD extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: BoomTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(core = tp.tileParams.core.copy(
      bpdMaxMetaLength = 120,
      globalHistoryLength = 64,
      localHistoryLength = 1,
      localHistoryNSets = 0,
      branchPredictor = ((resp_in: BranchPredictionBankResponse, p: Parameters) => {
        val loop = Module(new LoopBranchPredictorBank()(p))
        val tage = Module(new TageBranchPredictorBank()(p))
        val btb = Module(new BTBBranchPredictorBank()(p))
        val bim = Module(new BIMBranchPredictorBank()(p))
        val ubtb = Module(new FAMicroBTBBranchPredictorBank()(p))
        val preds = Seq(loop, tage, btb, ubtb, bim)
        preds.map(_.io := DontCare)

        ubtb.io.resp_in(0)  := resp_in
        bim.io.resp_in(0)   := ubtb.io.resp
        btb.io.resp_in(0)   := bim.io.resp
        tage.io.resp_in(0)  := btb.io.resp
        loop.io.resp_in(0)  := tage.io.resp

        (preds, loop.io.resp)
      })
    )))
    case other => other
  }
})

class WithBoom2BPD extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: BoomTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(core = tp.tileParams.core.copy(
      bpdMaxMetaLength = 45,
      globalHistoryLength = 16,
      localHistoryLength = 1,
      localHistoryNSets = 0,
      branchPredictor = ((resp_in: BranchPredictionBankResponse, p: Parameters) => {
        // gshare is just variant of TAGE with 1 table
        val gshare = Module(new TageBranchPredictorBank(
          BoomTageParams(tableInfo = Seq((256, 16, 7)))
        )(p))
        val btb = Module(new BTBBranchPredictorBank()(p))
        val bim = Module(new BIMBranchPredictorBank()(p))
        val preds = Seq(bim, btb, gshare)
        preds.map(_.io := DontCare)

        bim.io.resp_in(0)  := resp_in
        btb.io.resp_in(0)  := bim.io.resp
        gshare.io.resp_in(0) := btb.io.resp
        (preds, gshare.io.resp)
      })
    )))
    case other => other
  }
})

class WithAlpha21264BPD extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: BoomTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(core = tp.tileParams.core.copy(
      bpdMaxMetaLength = 64,
      globalHistoryLength = 32,
      localHistoryLength = 32,
      localHistoryNSets = 128,
      branchPredictor = ((resp_in: BranchPredictionBankResponse, p: Parameters) => {
        val btb = Module(new BTBBranchPredictorBank()(p))
        val gbim = Module(new HBIMBranchPredictorBank()(p))
        val lbim = Module(new HBIMBranchPredictorBank(BoomHBIMParams(useLocal=true))(p))
        val tourney = Module(new TourneyBranchPredictorBank()(p))
        val preds = Seq(lbim, btb, gbim, tourney)
        preds.map(_.io := DontCare)

        gbim.io.resp_in(0) := resp_in
        lbim.io.resp_in(0) := resp_in
        tourney.io.resp_in(0) := gbim.io.resp
        tourney.io.resp_in(1) := lbim.io.resp
        btb.io.resp_in(0)  := tourney.io.resp

        (preds, btb.io.resp)
      })
    )))
    case other => other
  }
})


class WithSWBPD extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => up(TilesLocated(InSubsystem), site) map {
    case tp: BoomTileAttachParams => tp.copy(tileParams = tp.tileParams.copy(core = tp.tileParams.core.copy(
      bpdMaxMetaLength = 1,
      globalHistoryLength = 32,
      localHistoryLength = 1,
      localHistoryNSets = 0,
      branchPredictor = ((resp_in: BranchPredictionBankResponse, p: Parameters) => {
        val sw = Module(new SwBranchPredictorBank()(p))

        sw.io.resp_in(0) := resp_in

        (Seq(sw), sw.io.resp)
      })
    )))
    case other => other
  }
})
