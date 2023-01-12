//See LICENSE for license details
package firesim.bridges

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.util._
import freechips.rocketchip.rocket.TracedInstruction
import freechips.rocketchip.tile.TileKey

import testchipip.{TileTraceIO, DeclockedTracedInstruction, TracedInstructionWidths}

import midas.targetutils.TriggerSource
import midas.widgets._
import testchipip.{StreamIO, StreamChannel}
import junctions.{NastiIO, NastiKey}

case class TracerVKey(
  insnWidths: TracedInstructionWidths, // Widths of variable length fields in each TI
  vecSize: Int // The number of insns in the traced insn vec (= max insns retired at that core) 
)


class TracerVTargetIO(insnWidths: TracedInstructionWidths, numInsns: Int) extends Bundle {
  val trace = Input(new TileTraceIO(insnWidths, numInsns))
  val triggerCredit = Output(Bool())
  val triggerDebit = Output(Bool())
}
/**
  * Target-side module for the TracerV Bridge.
  *
  * @param insnWidths A case class containing the widths of configurable-length
  * fields in the trace interface.
  *
  * @param numInsns The number of instructions captured in a single a cycle
  * (generally, the commit width of the pipeline)
  *
  * Warning: If you're not going to use the companion object to instantiate
  * this bridge you must call [[TracerVBridge.generateTriggerAnnotations] _in
  * the parent module_.
  */
class TracerVBridge(insnWidths: TracedInstructionWidths, numInsns: Int) extends BlackBox
    with Bridge[HostPortIO[TracerVTargetIO], TracerVBridgeModule] {
  val io = IO(new TracerVTargetIO(insnWidths, numInsns))
  val bridgeIO = HostPort(io)
  val constructorArg = Some(TracerVKey(insnWidths, numInsns))
  generateAnnotations()
  // Use in parent module: annotates the bridge instance's ports to indicate its trigger sources
  // def generateTriggerAnnotations(): Unit = TriggerSource(io.triggerCredit, io.triggerDebit)
  def generateTriggerAnnotations(): Unit =
    TriggerSource.evenUnderReset(WireDefault(io.triggerCredit), WireDefault(io.triggerDebit))

  // To placate CheckHighForm, uniquify blackbox module names by using the
  // bridge's instruction count as a string suffix. This ensures that TracerV
  // blackboxes with different instruction counts will have different defnames,
  // preventing FIRRTL CheckHighForm failure when using a chipyard "Hetero"
  // config. While a black box parameter relaxes the check on leaf field
  // widths, CheckHighForm does not permit parameterizations of the length of a
  // Vec enclosing those fields (as is the case here), since the Vec is lost in
  // a lowered verilog module.
  //
  // See https://github.com/firesim/firesim/issues/729.
  def defnameSuffix = s"_${numInsns}Wide_" + insnWidths.toString.replaceAll("[(),]", "_")

  override def desiredName = super.desiredName + defnameSuffix
}

object TracerVBridge {
  def apply(widths: TracedInstructionWidths, numInsns: Int)(implicit p: Parameters): TracerVBridge = {
    val tracerv = Module(new TracerVBridge(widths, numInsns))
    tracerv.generateTriggerAnnotations
    tracerv.io.trace.clock := Module.clock
    tracerv.io.trace.reset := Module.reset
    tracerv
  }

  // def apply(tracedInsns: TileTraceIO)(implicit p:Parameters): TracerVBridge = {
  //   val tracerv = withClockAndReset(tracedInsns.clock, tracedInsns.reset) {
  //     TracerVBridge(tracedInsns.insnWidths, tracedInsns.numInsns)
  //   }
  //   tracerv.io.trace := tracedInsns
  //   tracerv
  // }

  def apply(tracedInsns: TileTraceIO)(implicit p:Parameters): TracerVBridge = {
    val ep = Module(new TracerVBridge(tracedInsns.insnWidths, tracedInsns.numInsns))
    withClockAndReset(tracedInsns.clock, tracedInsns.reset) { ep.generateTriggerAnnotations() }
    ep.io.trace := tracedInsns
    ep
  }
}

class TracerVBridgeModule(key: TracerVKey)(implicit p: Parameters)
    extends BridgeModule[HostPortIO[TracerVTargetIO]]()(p)
    with StreamToHostCPU {

  // StreamToHostCPU  mixin parameters
  // Use the legacy NIC depth
  val toHostCPUQueueDepth  = TokenQueueConsts.TOKEN_QUEUE_DEPTH

  lazy val module = new BridgeModuleImp(this) {
    val io = IO(new WidgetIO)
    val hPort = IO(HostPort(new TracerVTargetIO(key.insnWidths, key.vecSize)))


    // Mask off valid committed instructions when under reset
    val traces = hPort.hBits.trace.insns.map({ unmasked =>
      val masked = WireDefault(unmasked)
      masked.valid := unmasked.valid && !hPort.hBits.trace.reset.asBool
      masked
    })
    private val pcWidth = traces.map(_.iaddr.getWidth).max
    private val insnWidth = traces.map(_.insn.getWidth).max
    val cycleCountWidth = 64

    // Set after trigger-dependent memory-mapped registers have been set, to
    // prevent spurious credits
    val initDone    = genWORegInit(Wire(Bool()), "initDone", false.B)
    // When unset, diables token capture to improve FMR, while still enabling the
    // use of TracerV-based triggers
    val traceEnable    = genWORegInit(Wire(Bool()), "traceEnable", true.B)
    //Program Counter trigger value can be configured externally
    val hostTriggerPCWidthOffset = pcWidth - p(CtrlNastiKey).dataBits
    val hostTriggerPCLowWidth = if (hostTriggerPCWidthOffset > 0) p(CtrlNastiKey).dataBits else pcWidth
    val hostTriggerPCHighWidth = if (hostTriggerPCWidthOffset > 0) hostTriggerPCWidthOffset else 0

    val hostTriggerPCStartHigh = RegInit(0.U(hostTriggerPCHighWidth.W))
    val hostTriggerPCStartLow = RegInit(0.U(hostTriggerPCLowWidth.W))
    attach(hostTriggerPCStartHigh, "hostTriggerPCStartHigh", WriteOnly)
    attach(hostTriggerPCStartLow, "hostTriggerPCStartLow", WriteOnly)
    val hostTriggerPCStart = Cat(hostTriggerPCStartHigh, hostTriggerPCStartLow)
    val triggerPCStart = RegInit(0.U(pcWidth.W))
    triggerPCStart := hostTriggerPCStart

    val hostTriggerPCEndHigh = RegInit(0.U(hostTriggerPCHighWidth.W))
    val hostTriggerPCEndLow = RegInit(0.U(hostTriggerPCLowWidth.W))
    attach(hostTriggerPCEndHigh, "hostTriggerPCEndHigh", WriteOnly)
    attach(hostTriggerPCEndLow, "hostTriggerPCEndLow", WriteOnly)
    val hostTriggerPCEnd = Cat(hostTriggerPCEndHigh, hostTriggerPCEndLow)
    val triggerPCEnd = RegInit(0.U(pcWidth.W))
    triggerPCEnd := hostTriggerPCEnd

    //Cycle count trigger
    val hostTriggerCycleCountWidthOffset = 64 - p(CtrlNastiKey).dataBits
    val hostTriggerCycleCountLowWidth = if (hostTriggerCycleCountWidthOffset > 0) p(CtrlNastiKey).dataBits else 64
    val hostTriggerCycleCountHighWidth = if (hostTriggerCycleCountWidthOffset > 0) hostTriggerCycleCountWidthOffset else 0

    val hostTriggerCycleCountStartHigh = RegInit(0.U(hostTriggerCycleCountHighWidth.W))
    val hostTriggerCycleCountStartLow = RegInit(0.U(hostTriggerCycleCountLowWidth.W))
    attach(hostTriggerCycleCountStartHigh, "hostTriggerCycleCountStartHigh", WriteOnly)
    attach(hostTriggerCycleCountStartLow, "hostTriggerCycleCountStartLow", WriteOnly)
    val hostTriggerCycleCountStart = Cat(hostTriggerCycleCountStartHigh, hostTriggerCycleCountStartLow)
    val triggerCycleCountStart = RegInit(0.U(cycleCountWidth.W))
    triggerCycleCountStart := hostTriggerCycleCountStart

    val hostTriggerCycleCountEndHigh = RegInit(0.U(hostTriggerCycleCountHighWidth.W))
    val hostTriggerCycleCountEndLow = RegInit(0.U(hostTriggerCycleCountLowWidth.W))
    attach(hostTriggerCycleCountEndHigh, "hostTriggerCycleCountEndHigh", WriteOnly)
    attach(hostTriggerCycleCountEndLow, "hostTriggerCycleCountEndLow", WriteOnly)
    val hostTriggerCycleCountEnd = Cat(hostTriggerCycleCountEndHigh, hostTriggerCycleCountEndLow)
    val triggerCycleCountEnd = RegInit(0.U(cycleCountWidth.W))
    triggerCycleCountEnd := hostTriggerCycleCountEnd

    val trace_cycle_counter = RegInit(0.U(cycleCountWidth.W))

    //target instruction type trigger (trigger through target software)
    //can configure the trigger instruction type externally though simulation driver
    val hostTriggerStartInst = RegInit(0.U(insnWidth.W))
    val hostTriggerStartInstMask = RegInit(0.U(insnWidth.W))
    attach(hostTriggerStartInst, "hostTriggerStartInst", WriteOnly)
    attach(hostTriggerStartInstMask, "hostTriggerStartInstMask", WriteOnly)

    val hostTriggerEndInst = RegInit(0.U(insnWidth.W))
    val hostTriggerEndInstMask = RegInit(0.U(insnWidth.W))
    attach(hostTriggerEndInst, "hostTriggerEndInst", WriteOnly)
    attach(hostTriggerEndInstMask, "hostTriggerEndInstMask", WriteOnly)

    //trigger selector
    val triggerSelector = RegInit(0.U((p(CtrlNastiKey).dataBits).W))
    attach(triggerSelector, "triggerSelector", WriteOnly)

    //set the trigger
    //assert(triggerCycleCountEnd >= triggerCycleCountStart)
    val triggerCycleCountVal = RegInit(false.B)
    triggerCycleCountVal := (trace_cycle_counter >= triggerCycleCountStart) & (trace_cycle_counter <= triggerCycleCountEnd)

    val triggerPCValVec = RegInit(VecInit(Seq.fill(traces.length)(false.B)))
    traces.zipWithIndex.foreach { case (trace, i) =>
      when (trace.valid) {
        when (triggerPCStart === trace.iaddr) {
          triggerPCValVec(i) := true.B
        } .elsewhen ((triggerPCEnd === trace.iaddr) && triggerPCValVec(i)) {
          triggerPCValVec(i) := false.B
        }
      }
    }

    val triggerInstValVec = RegInit(VecInit(Seq.fill(traces.length)(false.B)))
    traces.zipWithIndex.foreach { case (trace, i) =>
      when (trace.valid) {
        when (!((hostTriggerStartInst ^ trace.insn) & hostTriggerStartInstMask).orR) {
          triggerInstValVec(i) := true.B
        } .elsewhen (!((hostTriggerEndInst ^ trace.insn) & hostTriggerEndInstMask).orR) {
          triggerInstValVec(i) := false.B
        }
      }
    }

    val trigger = MuxLookup(triggerSelector, false.B, Seq(
      0.U -> true.B,
      1.U -> triggerCycleCountVal,
      2.U -> triggerPCValVec.reduce(_ || _),
      3.U -> triggerInstValVec.reduce(_ || _)))

    val mux_valid = RegInit(false.B)

    val tFireHelper = DecoupledHelper(streamEnq.ready, hPort.toHost.hValid, hPort.fromHost.hReady, initDone)

    val triggerReg = RegEnable(trigger, false.B, tFireHelper.fire())
    hPort.hBits.triggerDebit := !trigger && triggerReg
    hPort.hBits.triggerCredit := trigger && !triggerReg

    val uint_traces = (traces map (trace => Cat(trace.valid, trace.iaddr).pad(64))).reverse
    streamEnq.bits := Cat(uint_traces :+ trace_cycle_counter.pad(64)).pad(BridgeStreamConstants.streamWidthBits)

    val mux_hold = RegInit(false.B)

    hPort.toHost.hReady := tFireHelper.fire(hPort.toHost.hValid) && !mux_hold // WRONG MOVE HERE
    hPort.fromHost.hValid := tFireHelper.fire(hPort.fromHost.hReady)

    // streamEnq.valid := (tFireHelper.fire(streamEnq.ready, trigger) && traceEnable)
    streamEnq.valid := (trigger && traceEnable && initDone && mux_valid)  // WRONG MOVE HERE

    when (tFireHelper.fire()) {
      trace_cycle_counter := trace_cycle_counter + 1.U
    }

    val fsmT :: sNone :: sProcessing :: sArmValid :: sFooB :: sFooC :: sOther :: Nil = Enum(7)
    val fsm = RegInit(fsmT)
    val select: UInt = RegInit(0.U(5.W))

    val traces_A = Seq(traces(0), traces(1), traces(2), traces(3), traces(4), traces(5), traces(6))
    val uint_traces_A = (traces_A map (trace => Cat(trace.valid, trace.iaddr).pad(64))).reverse
    val stream_bits_A = Cat(uint_traces_A :+ trace_cycle_counter.pad(64)).pad(BridgeStreamConstants.streamWidthBits)
    val traces_A_decider = traces(0)
    
    val traces_B = Seq(traces(7), traces(8), traces(9), traces(10), traces(11), traces(12), traces(13))
    val uint_traces_B = (traces_B map (trace => Cat(trace.valid, trace.iaddr).pad(64))).reverse
    val stream_bits_B = Cat(uint_traces_B :+ trace_cycle_counter.pad(64)).pad(BridgeStreamConstants.streamWidthBits)
    val traces_B_decider = traces(7)
    
    // val traces_C = Seq(traces(14), traces(15), traces(16), traces(17), traces(18), traces(19), traces(20))
    // val uint_traces_C = (traces_C map (trace => Cat(trace.valid, trace.iaddr).pad(64))).reverse
    // val stream_bits_C = Cat(uint_traces_C :+ trace_cycle_counter.pad(64)).pad(BridgeStreamConstants.streamWidthBits)
    // val traces_C_decider = traces(14)
    
    val theMux = MuxLookup(select, stream_bits_A, Seq(
      0.U -> stream_bits_A,
      1.U -> stream_bits_B,
      // 2.U -> stream_bits_C
      ))
    val total_arms = RegInit(3.U)

    dontTouch(fsm)
    dontTouch(select)
    dontTouch(theMux)

    dontTouch(stream_bits_A)
    dontTouch(stream_bits_B)
    // dontTouch(stream_bits_C)
    
    when(hPort.hBits.trace.reset) {
      fsm := sNone
    }
    
      
    
    val use_arms = RegInit(0.U(5.W))
    val sent_arms = RegInit(0.U(5.W))
    

    dontTouch(use_arms)
    dontTouch(sent_arms)
    dontTouch(mux_valid)
    dontTouch(mux_hold)

    // when(!(use_arms === 0.U)) {
    //   mux_hold := true.B
    //   mux_valid := true.B
    // }.otherwise {
    //   mux_hold := false.B
    //   mux_valid := false.B
    // }

    switch(fsm) {
      is(sNone) {

        when(hPort.toHost.hValid === true.B) {
          when(traces_B_decider.valid) {
            use_arms := 2.U
            fsm := sArmValid
            // mux_hold := true.B
            // mux_valid := true.B
            // mux_valid := true.B
          }.elsewhen(traces_A_decider.valid) {
            use_arms := 1.U
            fsm := sArmValid
            // mux_hold := true.B
            // mux_valid := true.B
            // fsm := sArmValid
            // mux_valid := true.B
          }.otherwise {
            fsm := sNone
            mux_hold := false.B
            mux_valid := false.B
            // mux_hold := false.B
          }
          // fsm := sProcessing
          // use_arms := 0.U
          // sent_arms := 0.U
          // mux_valid := false.B
          // mux_hold := true.B
        }.otherwise {
          // mux_hold := false.B
        }
      }
      is(sArmValid) {
        when(!(use_arms === 0.U)) {
          select := use_arms - 1.U
          use_arms := use_arms - 1.U
          mux_hold  := true.B
          mux_valid := true.B
          // mux_hold  := !(use_arms === 1.U)
          // mux_valid := !(use_arms === 1.U)
        }.otherwise {
          fsm := sNone
          mux_hold := false.B
          mux_valid := false.B
        }
        // when(sent_arms === use_arms) {
        //   // done sending
        //   mux_valid := false.B
        //   fsm := sNone
        // }.otherwise {
        //   // send the next arm
        //   fsm := sArmValid
        //   mux_valid := true.B
        //   select := sent_arms + 1.U
        //   sent_arms := sent_arms + 1.U
        // }
      }

      // is(sProcessing) {
      //   select := 0.U
      //   when(traces_B_decider.valid) {
      //     use_arms := 1.U
      //     fsm := sArmValid
      //     mux_valid := true.B
      //   }.elsewhen((traces_A_decider.valid)) {
      //     use_arms := 0.U
      //     fsm := sArmValid
      //     mux_valid := true.B
      //   }.otherwise {
      //     fsm := sNone
      //     mux_hold := false.B
      //   }
      // }
      // is(sArmValid) {
      //   when(sent_arms === use_arms) {
      //     // done sending
      //     mux_valid := false.B
      //     fsm := sNone
      //   }.otherwise {
      //     // send the next arm
      //     fsm := sArmValid
      //     mux_valid := true.B
      //     select := sent_arms + 1.U
      //     sent_arms := sent_arms + 1.U
      //   }
      // }
      is(sFooB) {
        select := 1.U
        fsm := sFooC
      }
      is(sFooC) {
        select := 2.U
        fsm := sNone
      }
    }

      
    // state machine
    // mux traces onto streamEnq.bits
    // when toHost.valid is true, we have a new token -> processing state
    // processing state -> 
    // assume trace.valid is continugous , as soon as first non-valid is found, no remaining are valid

//                                       |
    // 7 are valid                vvvvvvv
    // 10 are valid               vvvvvvv vvv


    // count valids, deicde which arm of the mux to use
    // only need to check first valid bit per arm
    
    // strobe valid, and update streamEnq.bits

    // mux 32 / 7 inputs, 0-6 would use first arm of the mux

    // 1: v
    // 2: vv
    // 3: vvv

    genCRFile()
    override def genHeader(base: BigInt, sb: StringBuilder): Unit = {
      import CppGenerationUtils._
      val headerWidgetName = getWName.toUpperCase
      super.genHeader(base, sb)
      sb.append(genConstStatic(s"${headerWidgetName}_max_core_ipc", UInt32(traces.size)))
      emitClockDomainInfo(headerWidgetName, sb)
    }
  }
}
