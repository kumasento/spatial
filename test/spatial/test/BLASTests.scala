package spatial.test

import _root_.spatial.lib._
import nova.test.NovaTestbench
import spatial.dsl._
import utest._

@spatial object GEMM {
  def main(): Void = Accel {
    val M = 96  // Rows of output
    val N = 96  // Cols of output
    val K = 96  // Common dimension
    val a = SRAM[I32](M,K)
    val b = SRAM[I32](K,N)
    val y = SRAM[I32](M,N)
    gemm[I32](y,a,b,0,false,false,1,2)
  }
}

object BLASTests extends Testbench { val tests = Tests {
  'GEMM - test(GEMM)
}}
