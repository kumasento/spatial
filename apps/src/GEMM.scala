import spatial.dsl._

// NOTE: why this cannot compile?
@spatial object NaiveGEMM extends SpatialApp {
  def main(args: Array[String]): Void = {
    type T = FixPt[TRUE, _24, _8]

    val m = args(0).to[Int]
    val n = args(1).to[Int]
    val p = args(2).to[Int]

    val a = (0::m, 0::p) { (i, j) => ((i + j * p) % 8).to[T] }
    val b = (0::p, 0::n) { (i, j) => ((i + j * n) % 8).to[T] }
    val c_init = (0::m, 0::n) { (_, _) => 0.to[T] }

    val M = ArgIn[Int]
    val N = ArgIn[Int]
    val P = ArgIn[Int]
    setArg(M, m)
    setArg(N, n)
    setArg(P, p)

    val A = DRAM[T](M, P)
    val B = DRAM[T](P, N)
    val C = DRAM[T](M, N)
    setMem(A, a)
    setMem(B, b)
    setMem(C, c_init)

    Accel {
      val A_s = SRAM[T](m, p)
      val B_s = SRAM[T](p, n)
      val C_s = SRAM[T](m, n)

      A_s load A(0::m, 0::p)
      B_s load B(0::p, 0::n)
      C_s load C(0::m, 0::n)

      Foreach(M by 1, N by 1) { (i, j) => 
        C_s(i, j) = Reduce(Reg[T](0))(P by 1) { k => 
          A_s(i, k) * B_s(k, j) 
        }{_ + _}
      }

      C(0::m, 0::n) store C_s
    }

    val result = getMatrix(C)

    val gold = (0::m, 0::n) { (i, j) => 
      Array.tabulate(p) { k => 
        a(i, k) * b(k, j)
      }.reduce{_ + _}
    }

    // Show results
    println(r"expected cksum: ${gold.map(a => a).reduce{_+_}}")
    println(r"result cksum: ${result.map(a => a).reduce{_+_}}")
    printMatrix(gold, "Gold: ")
    printMatrix(result, "Result: ")
    assert(gold == result)
  }
}


@spatial object TiledGEMM extends SpatialApp {
  def main(args: Array[String]): Void = {
    type T = FixPt[TRUE, _24, _8]

    val m = args(0).to[Int]
    val n = args(1).to[Int]
    val p = args(2).to[Int]

    val a = (0::m, 0::p) { (i, j) => ((i + j * p) % 8).to[T] }
    val b = (0::p, 0::n) { (i, j) => ((i + j * n) % 8).to[T] }
    val c_init = (0::m, 0::n) { (_, _) => 0.to[T] }

    val M = ArgIn[Int]
    val N = ArgIn[Int]
    val P = ArgIn[Int]
    setArg(M, m)
    setArg(N, n)
    setArg(P, p)

    val A = DRAM[T](M, P)
    val B = DRAM[T](P, N)
    val C = DRAM[T](M, N)
    setMem(A, a)
    setMem(B, b)
    setMem(C, c_init)

    val bm = 4
    val bn = 4 
    val bp = 4

    setMem(A, a)
    setMem(B, b)
    setMem(C, c_init)

    Accel {
      Foreach(M by bm, N by bn) { (i, j) => 
        // NOTE: this .buffer/.nonbuffer flag seems to be necessary for the latest spatial version
        // Why there will be hazard? Maybe come from MemFold(tileC)(...) {...}{...}
        val tileC = SRAM[T](bm, bn).nonbuffer

        tileC load C(i::i+bm, j::j+bn)

        MemFold(tileC)(P by bp) { k => 

          val tileA = SRAM[T](bm, bp)
          val tileB = SRAM[T](bp, bn)
          val accum = SRAM[T](bm, bn)

          Parallel {
            tileA load A(i::i+bm, k::k+bp)
            tileB load B(k::k+bp, j::j+bn)
          }

          MemReduce(accum)(bp by 1) { kk => 
            val tileC_part = SRAM[T](bm, bn)
            Foreach(bm by 1, bn by 1) { (ii, jj) => 
              tileC_part(ii, jj) = tileA(ii, kk) * tileB(kk, jj)
            }
            tileC_part
          }{ _ + _ }
        }{ _ + _ }

        C(i::i+bm, j::j+bn) store tileC
      }
    }

    val result = getMatrix(C)

    val gold = (0::m, 0::n) { (i, j) => 
      Array.tabulate(p) { k => 
        a(i, k) * b(k, j)
      }.reduce{_ + _}
    }

    // Show results
    println(r"expected cksum: ${gold.map(a => a).reduce{_+_}}")
    println(r"result cksum: ${result.map(a => a).reduce{_+_}}")
    printMatrix(gold, "Gold: ")
    printMatrix(result, "Result: ")
    assert(gold == result)
  }
}
