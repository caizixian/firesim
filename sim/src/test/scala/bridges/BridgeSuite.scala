//See LICENSE for license details.

package firesim.bridges

import java.io._

import org.scalatest.Suites
import org.scalatest.matchers.should._

import freechips.rocketchip.config.Config
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.config._

import firesim.{BasePlatformConfig, TestSuiteCommon}

object BaseConfigs {
  case object F1    extends BasePlatformConfig("f1", Seq(classOf[DefaultF1Config]))
  case object Vitis extends BasePlatformConfig("vitis", Seq(classOf[DefaultVitisConfig]))
}

abstract class BridgeSuite(
  val targetName:                  String,
  override val targetConfigs:      String,
  override val basePlatformConfig: BasePlatformConfig,
) extends TestSuiteCommon("bridges")
    with Matchers {

  def run(
    backend:  String,
    debug:    Boolean      = false,
    logFile:  Option[File] = None,
    waveform: Option[File] = None,
    args:     Seq[String]  = Nil,
  ) = {
    val makeArgs = Seq(
      s"run-$backend%s".format(if (debug) "-debug" else ""),
      "LOGFILE=%s".format(logFile.map(toStr).getOrElse("")),
      "WAVEFORM=%s".format(waveform.map(toStr).getOrElse("")),
      "ARGS=%s".format(args.mkString(" ")),
    )
    if (isCmdAvailable(backend)) {
      make(makeArgs: _*)
    } else 0
  }

  /** Runs MIDAS-level simulation on the design.
    *
    * @param backend
    *   Backend simulator: "verilator" or "vcs"
    * @param debug
    *   When true, captures waves from the simulation
    */
  def runTest(backend: String, debug: Boolean = false)

  /** Helper to generate tests strings.
    */
  def getTestString(length: Int): String = {
    val gen   = new scala.util.Random(100)
    val alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    (1 to length).map(_ => alpha(gen.nextInt(alpha.length))).mkString
  }

  mkdirs()
  behavior.of(s"$targetName")
  elaborateAndCompile()
  for (backend <- Seq("vcs", "verilator")) {
    compileMlSimulator(backend)

    val testEnvStr = s"pass in ${backend} MIDAS-level simulation"

    if (isCmdAvailable(backend)) {
      it should testEnvStr in {
        runTest(backend)
      }
    } else {
      ignore should testEnvStr in {}
    }
  }
}

class UARTTest(targetConfig: BasePlatformConfig) extends BridgeSuite("UARTModule", "UARTConfig", targetConfig) {
  override def runTest(backend: String, debug: Boolean) {
    // Generate a short test string.
    val data = getTestString(16)

    // Create an input file.
    val input       = File.createTempFile("input", ".txt")
    input.deleteOnExit()
    val inputWriter = new BufferedWriter(new FileWriter(input))
    inputWriter.write(data)
    inputWriter.flush()
    inputWriter.close()

    // Create an output file to write to.
    val output = File.createTempFile("output", ".txt")

    val runResult = run(backend, debug, args = Seq(s"+uart-in0=${input.getPath}", s"+uart-out0=${output.getPath}"))
    assert(runResult == 0)
    val result    = scala.io.Source.fromFile(output.getPath).mkString
    result should equal(data)
  }
}

class UARTF1Test    extends UARTTest(BaseConfigs.F1)
class UARTVitisTest extends UARTTest(BaseConfigs.Vitis)

class BlockDevTest(targetConfig: BasePlatformConfig)
    extends BridgeSuite("BlockDevModule", "BlockDevConfig", targetConfig) {
  override def runTest(backend: String, debug: Boolean) {
    // Generate a random string spanning 2 sectors with a fixed seed.
    val data = getTestString(1024)

    // Create an input file.
    val input       = File.createTempFile("input", ".txt")
    input.deleteOnExit()
    val inputWriter = new BufferedWriter(new FileWriter(input))
    inputWriter.write(data)
    inputWriter.flush()
    inputWriter.close()

    // Pre-allocate space in the output.
    val output       = File.createTempFile("output", ".txt")
    output.deleteOnExit()
    val outputWriter = new BufferedWriter(new FileWriter(output))
    for (i <- 1 to data.size) {
      outputWriter.write('x')
    }
    outputWriter.flush()
    outputWriter.close()

    val runResult = run(backend, debug, args = Seq(s"+blkdev0=${input.getPath}", s"+blkdev1=${output.getPath}"))
    assert(runResult == 0)
    val result    = scala.io.Source.fromFile(output.getPath).mkString
    result should equal(data)
  }
}

class BlockDevF1Test    extends BlockDevTest(BaseConfigs.F1)
class BlockDevVitisTest extends BlockDevTest(BaseConfigs.Vitis)

abstract class TracerVTestBase(platformConfig: BasePlatformConfig, width: Int)
    extends BridgeSuite("TracerVModule", s"PlusArgsModuleTestCount${width}", platformConfig) {
  override def runTest(backend: String, debug: Boolean) {
    // Create an expected file.
    val expected = File.createTempFile("expected", ".txt")
    expected.deleteOnExit()

    // Create the output file. tracerv will always append '-C0' to the end of the plusarg
    val output     = File.createTempFile("output", ".txt-C0")
    output.deleteOnExit()
    val outputPath = output.getPath.substring(0, output.getPath.length() - 3)

    val runResult =
      run(backend, false, args = Seq(s"+tracefile=${outputPath}", s"+tracerv-expected-output=${expected.getPath}"))
    assert(runResult == 0)

    val expectedContents = scala.io.Source.fromFile(expected.getPath).mkString
    val outputContents   = scala.io.Source.fromFile(output.getPath).mkString
    outputContents should equal(expectedContents)
  }
}

class TracerVF1TestCount1  extends TracerVTestBase(BaseConfigs.F1, 1);
class TracerVF1TestCount6  extends TracerVTestBase(BaseConfigs.F1, 6);
class TracerVF1TestCount7  extends TracerVTestBase(BaseConfigs.F1, 7);
class TracerVF1TestCount14 extends TracerVTestBase(BaseConfigs.F1, 14);
class TracerVF1TestCount15 extends TracerVTestBase(BaseConfigs.F1, 15);
class TracerVF1TestCount32 extends TracerVTestBase(BaseConfigs.F1, 32);
class TracerVVitisTest     extends TracerVTestBase(BaseConfigs.F1, 15);

class BridgeTests
    extends Suites(
      new UARTF1Test,
      new UARTVitisTest,
      new BlockDevF1Test,
      new BlockDevVitisTest,
      new TracerVF1TestCount1,
      new TracerVF1TestCount6,
      new TracerVF1TestCount7,
      new TracerVF1TestCount14,
      new TracerVF1TestCount15,
      new TracerVF1TestCount32,
      new TracerVVitisTest,
    )
