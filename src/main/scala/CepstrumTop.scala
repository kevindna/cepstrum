import chisel3._
import chisel3.util._
import chisel3.stage.ChiselStage
import chisel3.experimental.ChiselEnum
import java.io.FileWriter
import java.io.File
import fftgenerator._


//case class BasicFFTConfig(
//  n: Int,
//  pipelineDepth: Int,
//  lanes: Int,
//) extends FFTConfig[FixedPoint] {
//  val genIn = DspReal()
//  val genOut = DspComplex(SInt(32.W), SInt(32.W))
//}


//object FFTTop {
//	def main(args: Array[String]) {
//p
//    val fft = new FFT[DspReal](BasicFFTConfig(64, 6, 64))
//
//    //val test = (new ChiselStage).emitVerilog(new FFT[DspReal](BasicFFTConfig(64, 6, 64))
//    val test = (new ChiselStage).emitVerilog(new FFT[DspReal](BasicFFTConfig(64, 6, 64)), Array("--target-dir", "output/"))
//  }
//}

object Main {
  def main(args: Array[String]) : Unit = {
    println("HI")

    val cepstrum_params = FixedFFTConfig( IOWidth = 16,
      binaryPoint = 8,
      n = 32,
      pipelineDepth = 0,
      lanes = 32,
      inverse = false
    ) 

    (new ChiselStage).emitVerilog(new Cepstrum(cepstrum_params), 
                                  Array("--target-dir", "/scratch/kevinand/projects/cepstrum/src/"))
  }
}
