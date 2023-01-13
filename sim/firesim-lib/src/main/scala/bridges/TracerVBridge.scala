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

    val mux_valid = WireDefault(false.B)

    val tFireHelper = DecoupledHelper(streamEnq.ready, hPort.toHost.hValid, hPort.fromHost.hReady, initDone)

    val triggerReg = RegEnable(trigger, false.B, tFireHelper.fire())
    hPort.hBits.triggerDebit := !trigger && triggerReg
    hPort.hBits.triggerCredit := trigger && !triggerReg

    val uint_traces = (traces map (trace => Cat(trace.valid, trace.iaddr).pad(64))).reverse

    val destroy_token = WireDefault(false.B)

    // every time hPort.toHost.hValid asserts, I look at it do work, and ->
    // every time I assert hPort.toHost.hReady I am destroying the input token

    hPort.toHost.hReady := destroy_token
    // hPort.toHost.hReady := tFireHelper.fire(hPort.toHost.hValid) && !mux_hold // WRONG MOVE HERE
    hPort.fromHost.hValid := tFireHelper.fire(hPort.fromHost.hReady)

    // streamEnq.valid := (tFireHelper.fire(streamEnq.ready, trigger) && traceEnable)
    streamEnq.valid := (trigger && traceEnable && initDone && mux_valid)

    when (destroy_token) {
      trace_cycle_counter := trace_cycle_counter + 1.U
    }

    val armWidth = 7

    // divide with a ceiling round, to get the total number of arms
    val armCount = (traces.length + armWidth - 1) / armWidth

    // remove this duplicate representation
    val total_arms = 3.U

    val armWidths = Seq.tabulate(armCount)(x => math.min(traces.length - (x * armWidth), armWidth))

    println(s"AAAarmCount ${armCount}")
    println(s"WWWarmWidths ${armWidths}")

    // A Seq of Seq which represents each arm of the mux
    val allTraceArms = Seq(
      Seq(traces(0), traces(1), traces(2), traces(3), traces(4), traces(5), traces(6)),
      Seq(traces(7), traces(8), traces(9), traces(10), traces(11), traces(12), traces(13)),
      Seq(traces(14))
    )

    val allUintTraces = Seq(
      (allTraceArms(0) map (trace => Cat(trace.valid, trace.iaddr).pad(64))).reverse,
      (allTraceArms(1) map (trace => Cat(trace.valid, trace.iaddr).pad(64))).reverse,
      (allTraceArms(2) map (trace => Cat(trace.valid, trace.iaddr).pad(64))).reverse,
    )

    // Literally each arm of the mux, these are directly the bits that get put into the bump
    val allStreamBits = Seq(
      Cat(allUintTraces(0) :+ trace_cycle_counter.pad(64)).pad(BridgeStreamConstants.streamWidthBits),
      Cat(allUintTraces(1) :+ trace_cycle_counter.pad(64)).pad(BridgeStreamConstants.streamWidthBits),
      Cat(allUintTraces(2) :+ trace_cycle_counter.pad(64)).pad(BridgeStreamConstants.streamWidthBits),
    )

    // a parallel set of arms to a parallel mux, true if any instructions in the arm are valid 
    val allAnyValid = Seq(
      (allTraceArms(0).map(trace => trace.valid).reduce((a,b) => (a|b))),
      (allTraceArms(1).map(trace => trace.valid).reduce((a,b) => (a|b))),
      (allTraceArms(2).map(trace => trace.valid).reduce((a,b) => (a|b))),
    )

    val traces_A = Seq(traces(0), traces(1), traces(2), traces(3), traces(4), traces(5), traces(6))
    val uint_traces_A = (traces_A map (trace => Cat(trace.valid, trace.iaddr).pad(64))).reverse
    val stream_bits_A = Cat(uint_traces_A :+ trace_cycle_counter.pad(64)).pad(BridgeStreamConstants.streamWidthBits)
    val traces_A_any_valid = (traces_A.map(trace => trace.valid).reduce((a,b) => (a|b)))
    
    val traces_B = Seq(traces(7), traces(8), traces(9), traces(10), traces(11), traces(12), traces(13))
    val uint_traces_B = (traces_B map (trace => Cat(trace.valid, trace.iaddr).pad(64))).reverse
    val stream_bits_B = Cat(uint_traces_B :+ trace_cycle_counter.pad(64)).pad(BridgeStreamConstants.streamWidthBits)
    val traces_B_any_valid = (traces_B.map(trace => trace.valid).reduce((a,b) => (a|b)))
    
    // val traces_C = Seq(traces(14), traces(15), traces(16), traces(17), traces(18), traces(19), traces(20))
    // val uint_traces_C = (traces_C map (trace => Cat(trace.valid, trace.iaddr).pad(64))).reverse
    // val stream_bits_C = Cat(uint_traces_C :+ trace_cycle_counter.pad(64)).pad(BridgeStreamConstants.streamWidthBits)
    // val traces_C_decider = traces(14)

    val counter = RegInit(0.U(5.W))
    
    val theMux = MuxLookup(counter, allStreamBits(0), Seq(
      0.U -> allStreamBits(0),
      1.U -> allStreamBits(1),
      2.U -> allStreamBits(2),
      ))
    

    val anyValidMux = MuxLookup(counter, allAnyValid(0), Seq(
      0.U -> allAnyValid(0),
      1.U -> allAnyValid(1),
      2.U -> allAnyValid(2),
      ))

    streamEnq.bits := theMux

    mux_valid := (hPort.toHost.hValid && anyValidMux) && (counter =/= total_arms)

    when(mux_valid) {
      when(counter =/= total_arms) {
          counter := counter + 1.U
        } .otherwise {
          counter := 0.U
          destroy_token := true.B
        }
      } .otherwise {
      counter := 0.U
    }

    when(hPort.toHost.hValid && !mux_valid) {
      destroy_token := true.B
    }

    dontTouch(counter)
    dontTouch(theMux)
    dontTouch(anyValidMux)
    dontTouch(destroy_token)

    dontTouch(stream_bits_A)
    dontTouch(stream_bits_B)
    // dontTouch(stream_bits_C)
    
      
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
