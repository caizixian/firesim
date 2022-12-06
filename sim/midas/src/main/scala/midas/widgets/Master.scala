// See LICENSE for license details.

package midas
package widgets

import junctions._

import chisel3._
import chisel3.util.{Decoupled, Counter, log2Up}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

class SimulationMasterIO(implicit val p: Parameters) extends WidgetIO()(p){
}

class SimulationMaster(implicit p: Parameters) extends Widget()(p) {
  lazy val module = new WidgetImp(this) {
    val io = IO(new SimulationMasterIO)

    val initDelay = RegInit(64.U)
    when (initDelay =/= 0.U) { initDelay := initDelay - 1.U }
    genRORegInit(initDelay === 0.U, "INIT_DONE", 0.U)

    genCRFile()

    override def genHeader(base: BigInt, sb: StringBuilder): Unit = {
      super.genHeader(base, sb)

      genInclude(sb, "master")

      sb.append(s"#ifdef GET_CORE_CONSTRUCTOR\n")
      sb.append(s"registry.add_widget(new master_t(\n")
      sb.append(s"  simif,\n  ")
      crRegistry.genSubstructCreate(base, sb, "SIMULATIONMASTER")
      sb.append(s"));\n")
      sb.append(s"#endif // GET_CORE_CONSTRUCTOR\n")
    }
  }
}
