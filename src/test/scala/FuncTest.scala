import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import dsptools._
import dsptools.numbers._
import dsptools.numbers.implicits._
import dspjunctions._
import dspblocks._
import scala.math._
import fixedpoint._
import fixedpoint.{fromIntToBinaryPoint}
import fftgenerator._
// import chisel3.testers.{PeekPokeTester}
import chisel3.iotesters.{ChiselFlatSpec, Exerciser, PeekPokeTester, SteppedHWIOTester}


class BasicTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Cepstrum"
  // test class body here

  it should "do something" in {
  // test case body here

    val double_in = Seq(14.27, 84.66, 56.29, 80.99, 
                        79.98, 65.96, 1.31, 61.53, 
                        28.8, 57.50, 67.5, 89.68, 
                        81.72, 16.82, 88.4, 55.33, 
                        13.51, 87.78, 38.23, 76.60, 
                        3.42, 47.7, 11.48, 44.71, 
                        25.94, 30.69, 70.64, 97.32, 
                        75.21, 58.86, 17.85, 91.19)



    test(new Cepstrum(new FixedFFTConfig( IOWidth = 16,
                                          binaryPoint = 8,
                                          n = 32,
                                          pipelineDepth = 0,
                                          lanes = 32,
                                          inverse = false) )) { c =>

      // val cepstrum_in = Wire(Vec(32, DspReal()))
      // val data = double_in.map( x => DspReal(x) )
      // cepstrum_in := data
      // println(cepstrum_in)

      // test body here
      // c.io.in.poke(double_in.map( x => DspReal(x) ))
      PeekPokeTester.poke(c.io.in, double_in.map( x => DspReal(x) ))
      c.clock.step()
      c.clock.step()
      c.clock.step()
      c.clock.step()
      // println("Last output value :" + c.io.out.peek().litValue)

    }


  }
}