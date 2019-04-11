import spatial.dsl._

@spatial object GEMM extends SpatialApp {
  type X = FixPt[TRUE,_16,_16]

  def main(args: Array[String]): Unit = {
    // Get sizes for matrices from command line
    val m = args(0).to[Int]
    val n = args(1).to[Int]
    val p = args(2).to[Int]

    // Generate data for input matrices A and B, and initialize C
    val a = (0::m, 0::p){(i,j) => ((i + j * p) % 8).to[X] }
    val b = (0::p, 0::n){(i,j) => ((i + j * n) % 8).to[X] }
    val c_init = (0::m, 0::n){(_,_) => 0.to[X] }

    // Communicate dimensions to FPGA with ArgIns
    val M = ArgIn[Int]
    val N = ArgIn[Int]
    val P = ArgIn[Int]
    setArg(M,m)
    setArg(N,n)
    setArg(P,p)

    // Create pointers to matrices
    val A = DRAM[X](M, P)
    val B = DRAM[X](P, N)
    val C = DRAM[X](M, N)

    // Set up parallelizations
    val op = 1 // number of parallel blocks across M and N dims
    val mp = 1 // number of mul
    val ip = 1

    // tile sizes
    val bm = 16
    val bn = 64
    val bp = 64

    // initialize the contents in off-chip memory blocks
    setMem(A, a)
    setMem(B, b)
    setMem(C, c_init)

    Accel {
      Foreach(M by bm par op, N by bn par op) { (i, j) =>
        // create tiles on the board
        val tileC = SRAM[X](bm, bn)

        // 1st stage pipeline - prefetch C tile
        // par ip means collecting
        tileC load C(i::i+bm, j::j+bn par ip)

        // 2nd stage pipeline - load data into tileA and tileB
        MemFold(tileC)(P by bp){ k =>
          val tileA = SRAM[X](bm, bp)
          val tileB = SRAM[X](bp, bn)
          val accum = SRAM[X](bm, bn)

          Parallel {
            tileA load A(i::i+bm, k::k+bp)
            tileB load B(i::i+bm, k::k+bp)
          }

          // Returns a partial matrix multiplication result of a single P block
          MemReduce(accum)(bp by 1 par mp){ kk =>
            val tileC_partial = SRAM[X](bm, bn)

            // Element-wise multiplication
            Foreach(bm by 1, bn by 1 par ip){ (ii, jj) =>
              tileC_partial(ii, jj) = tileA(ii, kk) * tileB(kk, jj)
            }
            tileC_partial
          }{_ + _} // partial results are accumulated altogether

        }{_ + _} // Results from multiple blocks are accumulated together.


        // 3rd stage - store result back to DRAM
        C(i::i+bm, j::j+bn par ip) store tileC
      }
    }
  }
}
