/*
 * Author: Kevin Anderson (kevinand)
 * Date: May 26, 2024
 * Description: Simple module to implement LUT for LOG2
 *              Motivation is that chisel makes muxes out of 
 *              VecInit (in general Chisel does not do well 
 *              with memories)
 */

module Log2LUT(
  parameter WIDTH = 16,
  parameter BP    = 8,
  parameter DEPTH = 32) 
  (
  input  [$clog2(DEPTH)-1:0] index0,
  input  [$clog2(DEPTH)-1:0] index1,
  output [WIDTH-1:0] out0,
  output [WIDTH-1:0] out1);

  reg [WIDTH-1:0] lut [DEPTH-1:0];

  initial begin
    integer i;

    lut[0] = -WIDTH'd100 << BP;
    for(i = 1; i < DEPTH; i=i+1) begin
      lut[i] = $realtobits($log10(i)/$log10(2) * $pow(2, BP))[WIDTH-1:0];
    end 

  end

  always @* begin
    out0 <= lut[index];
    out1 <= lut[index];
  end


endmodule
