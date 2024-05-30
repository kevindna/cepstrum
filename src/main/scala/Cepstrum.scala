import chisel3.util._
import chisel3._
import craft._
import dsptools._
import dsptools.numbers._
import dsptools.numbers.implicits._
import dspjunctions._
import dspblocks._
import scala.math._
import fixedpoint._
import fixedpoint.{fromIntToBinaryPoint}
import fixedpoint.FixedPoint.Implicits.{fromDoubleToLiteral, fromBigDecimalToLiteral}
import fftgenerator._
import chisel3.internal.firrtl.{KnownWidth, UnknownWidth, Width}

import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.subsystem.{BaseSubsystem, PBUS}
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink.{TLRegisterNode, TLFragmenter}
import chisel3.experimental.BundleLiterals._
import chisel3.util.HasBlackBoxResource




/*
 * Author: kevinand
 *
 * Description: This module computes the cepstrum utilizing 
 *              the FFT generator by Steven Baily (now part
 *              of chipyard). The implementation is basic
 *
 * Log computation is based upon quantization to reduce LUT 
 * table size. This is a good reference, but not the exact 
 * reference.
 *
 * Note: the magnitude is never take because the magnitudes
 *       are squared. Instead, we simply compute the sum-of-
 *       squares.
 */


// class Log2LUTBlackBox(params: Map[String, Param] = Map.empty[String, Param]) extends BlackBox(params) with HasBlackBoxResource {
//   val io = IO(new Bundle {
//     val in0  = Input(UInt(64.W))
//     val in1  = Input(UInt(64.W))
//     val out0 = Output(UInt(64.W))
//     val out1 = Output(UInt(64.W))
//   })

//   addResource("../../resources/vsrc/log2lut.v")
// }


// TODO: Define Cepstrum IO bundle
class Cepstrum(params: FixedFFTConfig) extends Module {
  val io = IO(new Bundle{ 
                val in      = Input(Vec(params.n, params.genIn))
                val dbg_out = Output(ValidWithSync(Vec(params.n, params.genOut)))
                val out     = Output(ValidWithSync(Vec(params.n, FixedPoint(params.IOWidth.W, params.binaryPoint.BP))))
              })

  // Extract fixed point parameters
  val fp_w  = params.IOWidth.W
  val fp_bp = params.binaryPoint.BP

  //***********************//
  // Constants
  //***********************//
  val QUANT_BIT = 8
  val LOGOFZERO = -10.F(fp_w, fp_bp)
  val LOG2LUT_LIMIT = VecInit((pow(2,QUANT_BIT-1).toInt until pow(2,QUANT_BIT).toInt + 1).map(   // Allowable range plus one for interpolation
    x => {
      val sf = pow(2,-(QUANT_BIT-1)) // Scale factor for quantization
      RegInit((log(x*sf)/log(2)).F(fp_w, fp_bp))
    }))




  // Generate Parameters
  // val log_params =  Map("WIDTH" -> params.IOWidth, // If using a blackboxed LOG LUT 
  //                       "BP" -> params.binaryPoint,
  //                       "DEPTH" -> pow(2, QUANT_BIT))
  val ifft_params = new FixedFFTConfig( params.IOWidth, params.binaryPoint,
                                        params.n, params.pipelineDepth,
                                        params.lanes, true) 
  val unscrambled_params = FixedUnscrambleParams(params.IOWidth, params.binaryPoint, params.n)



  //***********************//
  // Instantiations
  //***********************//
  val fft_block   = Module(new FFT(params))
  val ifft_block  = Module(new FFT(ifft_params))
  // val log_lut     = Module(new Log2LUTBlackBox(log_params))
  val unscramble0 = Module(new Unscramble(unscrambled_params)).io
  val unscramble1 = Module(new Unscramble(unscrambled_params)).io


  // Take absolute value and compute logarithm (log2)
  def abslog(x : DspComplex[FixedPoint]) : DspComplex[FixedPoint] = {
    val sq  = x.abssq()
    val log = complog2(sq, QUANT_BIT).asSInt.asTypeOf(FixedPoint(fp_w, fp_bp))
    val res = Wire(DspComplex(FixedPoint(fp_w, fp_bp), FixedPoint(fp_w, fp_bp)))

    // Set fields
    res.real := log
    res.imag := 0.F(fp_w, fp_bp)

    return res
  }

  // Compute the logarithm of an input using LUT and interpolation.
  // Exploits binary and log are base 2. The log2 of a binary fixed
  // point number is log2(x) = log2(2^bp * integer) = bp + log2(intger)
  // log2 LUT holds values for log2(integer) where integer has width 
  // qb
  //  x : the value
  //  qb: the number of MSbs used to quantize x and index LUT
  def complog2(x: FixedPoint, qb : Int) : FixedPoint = {
    val X_LEN           = 2*params.IOWidth
    // val (quotient, rem) = x.getWidth /% 8
    val NUM_OF_BYTES    = ceil(x.getWidth/8).toInt
    val shft_tot        = WireInit(0.U(log2Ceil(X_LEN-1).toInt.W))
    val shft_amt        = Wire(Vec(NUM_OF_BYTES, UInt(shft_tot.getWidth.W)))
    val int_width       = (x.getWidth - x.binaryPoint.get) //.F(log2Ceil(x.getWidth + 1).W, 0.BP)

    // This comment applies to all code below:
    //    Normalize all value to 1 <= x < 2 and calculate the number 
    //    of shifts (scale factor) to normalize the values (Note: this
    //    is dynamic and the scale factor changes with each number ) 

    // Loop through all bytes to find number of left shifts needed to
    // normalize
    for (i <- NUM_OF_BYTES-1 to 0 by -1) {
      val byte      = Wire(UInt(8.W))         // Extract byte
      val mod8      = x.getWidth % 8  // Constant used to pad 
      // val mod8      = (2*params.IOWidth) % 8  // Constant used to pad 
      val msb_shft  = Wire(UInt(shft_tot.getWidth.W))
      val x_pad     = Wire(UInt((2*params.IOWidth).W)) 

      if (mod8 == 0) {
        x_pad :=  x.asTypeOf(x_pad)
      }  else {
        x_pad := Cat(x.asTypeOf(UInt(x.getWidth.W)), 0.U(mod8.W)).asTypeOf(x_pad)
      }

      byte := x_pad((i+1)*8-1, i*8).asTypeOf(byte)

      // Mux-chain to find how many left shifts needed to push
      // highest set bit to MSb position
      msb_shft := 7.U // Default case
      for (b <- 1 to 7) {
        when(byte(b) === 1.U) { msb_shft := (7-b).U }
      }

      // Translate byte-level shift to full-word shift (i.e. number of
      // shifts to move highest set bit in byte to the MSb of full word)
      when (byte === 0.U) {
        shft_amt(i) := (pow(2, msb_shft.getWidth)-1).toInt.U
      } .otherwise {
        shft_amt(i) := msb_shft + 8.U*(NUM_OF_BYTES-1-i).U
      }
    }

    // Select the lowest number of shifts to point the highest set bit
    // in word to word MSb
    shft_tot := shft_amt.reduceTree((a,b) => { Mux(a < b, a, b) })

    val x_shftd   = (x << shft_tot)(X_LEN-1, 0)                                             // Normalize the value
    val index     = x_shftd(X_LEN-1, X_LEN - qb) - pow(2, qb-1).toInt.U                     // Calculate the log LUT index
    val frac      = Cat(0.U(1.W),x_shftd(X_LEN-qb-1, 0)).asTypeOf(FixedPoint((X_LEN-qb+1).W, (X_LEN-qb).BP)) // Extract LSbs and interpret as fraction
    val intrep    = Wire(FixedPoint((X_LEN-qb+LOG2LUT_LIMIT(0).getWidth).W, (X_LEN-qb).BP)) // Perform interpolation
    val log_sf    = (0.S ## int_width.S).zext  - (0.U ## shft_tot).zext - 1.S(4.W)          // Calculate scale factor (2^X) to subtract after indexing LUT

    // Interpolate between current and next log2 entry by intrepeting LSbs as fractiional
    intrep := frac*(LOG2LUT_LIMIT(index+1.U) - LOG2LUT_LIMIT(index))

    // If input is zero, then output hardcode safe value. Otherwise, output true log approxiamation
    return Mux(x_shftd === 0.U, 
        LOGOFZERO, 
        LOG2LUT_LIMIT(index) + 
        intrep(intrep.getWidth-1, intrep.getWidth-intrep.binaryPoint.get).asTypeOf(LOG2LUT_LIMIT(0)) +
        Cat(Fill((LOG2LUT_LIMIT(0).getWidth - LOG2LUT_LIMIT(0).binaryPoint.get - log2Ceil(X_LEN-1).toInt), log_sf.head(1)), log_sf,  0.U(LOG2LUT_LIMIT(0).binaryPoint.get.W)).asTypeOf(LOG2LUT_LIMIT(0))
      ) 
  }



  fft_block.io.in.sync  := false.B
  fft_block.io.in.valid := true.B
  fft_block.io.in.bits  := io.in

  unscramble0.out.ready := true.B
  unscramble0.in <> fft_block.io.out


  val bypass_fft = false.B

  when (bypass_fft) {
    ifft_block.io.in.sync   := false.B
    ifft_block.io.in.valid  := true.B
    ifft_block.io.in.bits   := io.in
  } .otherwise {
    ifft_block.io.in.sync   := false.B
    ifft_block.io.in.valid  := unscramble0.out.valid
    ifft_block.io.in.bits   := unscramble0.out.bits.map(x => abslog(x))
  }
  
  unscramble1.out.ready := true.B
  unscramble1.in <> ifft_block.io.out

  // Divide by 1/N (1/params.n) (ASSUMES params.n is power of 2)
  val ifft      = Wire(Vec(params.n, params.genOut))
  val cepstrum  = Wire(Vec(params.n, FixedPoint(fp_w, fp_bp)))
  for (i <- 0 until params.n) {
    ifft(i).real := (unscramble1.out.bits(i).real >> log2Ceil(params.n))
    ifft(i).imag := (unscramble1.out.bits(i).imag >> log2Ceil(params.n))
  }

  // Output
  for (i <- 0 until params.n) {
    cepstrum(i) := ifft(i).abssq() 
  }

  // Compute square (square handles outer abs) and multiply by 4 (performed with shifts)
  io.out.bits  := cepstrum
  io.out.valid := unscramble1.out.valid
  io.out.sync  := false.B 

  // Debug output
  io.dbg_out.valid    := true.B
  io.dbg_out.sync     := false.B 
  io.dbg_out.bits := unscramble1.out.bits

}


// WASTE

// // Compute the logarithm of an input using LUT and interpolation.
// // Exploits binary and log are base 2. The log2 of a binary fixed
// // point number is log2(x) = log2(2^bp * integer) = bp + log2(intger)
// // log2 LUT holds values for log2(integer) where integer has width 
// // qb
// //  x : the value
// //  qb: the number of MSbs used to quantize x and index LUT
// def complog(x: FixedPoint, qb : Int) : FixedPoint = {
//   val lut_index  = Wire(UInt(qb.W))                   // Index into log2 LUT
//   val intrep     = Wire(FixedPoint(fp_w, fp_bp))      // Fractional interpolation
//   val quant_frac = Wire(FixedPoint((params.IOWidth - qb + 1).W , (params.IOWidth - qb).BP)) // LSbs interpreted as fractional

//   lut_index  := x(2*params.IOWidth-1, 2*params.IOWidth-1-QUANT_BIT) // - pow(2,qb).toInt.U + 1.U
//   quant_frac := Cat(0.U(1.W) ,x(2*qb - 1, 0)).asTypeOf(quant_frac)
//   intrep     := quant_frac*(LOG2LUT(lut_index+1.U) - LOG2LUT(lut_index))

//   return LOG2LUT(lut_index) + intrep - params.binaryPoint.F(fp_w, fp_bp)
// }
