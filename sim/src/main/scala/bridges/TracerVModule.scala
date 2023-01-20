//See LICENSE for license details.

package firesim.bridges

import chisel3._

import firesim.midasexamples.PeekPokeMidasExampleHarness
import freechips.rocketchip.config.{Config, Field, Parameters}
import testchipip.DeclockedTracedInstruction
import testchipip.TracedInstructionWidths
import midas.targetutils.TriggerSink
import testchipip.TileTraceIO

class TracerVDUTIO(insnWidths: TracedInstructionWidths, numInsns: Int) extends Bundle {
  val triggerSink = Output(Bool())
  val insns       = Input(Vec(numInsns, new DeclockedTracedInstruction(insnWidths)))
}

class PlusArgsModuleTestCount1
    extends Config((site, here, up) => {
      case TracerVModuleInstructionCount => 1
      case TracerVModuleInstructionWidth => 40
    })

class PlusArgsModuleTestCount6
    extends Config((site, here, up) => {
      case TracerVModuleInstructionCount => 6
      case TracerVModuleInstructionWidth => 40
    })

class PlusArgsModuleTestCount7
    extends Config((site, here, up) => {
      case TracerVModuleInstructionCount => 7
      case TracerVModuleInstructionWidth => 40
    })

class PlusArgsModuleTestCount14
    extends Config((site, here, up) => {
      case TracerVModuleInstructionCount => 14
      case TracerVModuleInstructionWidth => 40
    })

class PlusArgsModuleTestCount15
    extends Config((site, here, up) => {
      case TracerVModuleInstructionCount => 15
      case TracerVModuleInstructionWidth => 40
    })

class PlusArgsModuleTestCount32
    extends Config((site, here, up) => {
      case TracerVModuleInstructionCount => 32
      case TracerVModuleInstructionWidth => 40
    })

case object TracerVModuleInstructionCount extends Field[Int]
case object TracerVModuleInstructionWidth extends Field[Int]

class TracerVDUT(implicit val p: Parameters) extends Module {

  val insnCount  = p(TracerVModuleInstructionCount)
  val insnWidth  = p(TracerVModuleInstructionWidth)
  val insnWidths = TracedInstructionWidths(insnWidth, insnCount, None, 64, 40)

  val io = IO(new TracerVDUTIO(insnWidths, insnCount))

  val tracerV = TracerVBridge(insnWidths, insnCount)
  tracerV.io.trace.insns := io.insns
  TriggerSink(io.triggerSink)
}

class TracerVModule(implicit p: Parameters) extends PeekPokeMidasExampleHarness(() => new TracerVDUT)
