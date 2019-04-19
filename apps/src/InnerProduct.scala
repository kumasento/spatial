import spatial.dsl._

@spatial object InnerProduct extends SpatialApp {

  def main(args: Array[String]): Unit = {
    // define the data type
    type T = FixPt[TRUE, _24, _8]

    // set vector length
    val len = 64

    // generate test data
    val vec1 = Array.tabulate[T](len) { i => i.to[T] }
    val vec2 = Array.tabulate[T](len) { i => (len.to[Int] - i).to[T] }

    // create the external memory
    val d1 = DRAM[T](len)
    val d2 = DRAM[T](len)

    // fill the external memory with generated test data
    setMem(d1, vec1)
    setMem(d2, vec2)

    // allocate register for the answer
    val x = ArgOut[T]
    Accel {
      // create local SRAMs
      val s1 = SRAM[T](len)
      val s2 = SRAM[T](len)

      // load data
      s1 load d1
      s2 load d2

      // multiply and accumulate
      x := Reduce(Reg[T](0))(len by 1) { i => s1(i) * s2(i) } {
        _ + _
      }
    }

    val gold = vec1.zip(vec2) {
      _ * _
    }.reduce {
      _ + _
    }

    assert(gold == getArg(x), r"Expected ${gold}, got ${getArg(x)}!")

    println("Test PASSED!")
  }
}

@spatial object TiledInnerProduct extends SpatialApp {

  def main(args: Array[String]): Unit = {
    // data type
    type T = FixPt[TRUE, _24, _8]

    val tileSize = 64

    // length of the input vector
    val len = ArgIn[Int]
    setArg(len, args(0).to[Int])

    val vec1 = Array.tabulate[T](len) { i => i.to[T] }
    val vec2 = Array.tabulate[T](len) { i => (len - i).to[T] }

    // DRAM to store input data
    val d1 = DRAM[T](len)
    val d2 = DRAM[T](len)

    setMem(d1, vec1)
    setMem(d2, vec2)


    val x = ArgOut[T]

    Accel {
      val s1 = SRAM[T](tileSize) // a single tile
      val s2 = SRAM[T](tileSize)

      x := Reduce(Reg[T](0))(len by tileSize) { tile =>
        // tile is the id of the current tile
        s1 load d1(tile :: tile + tileSize)
        s2 load d2(tile :: tile + tileSize)

        // dot-product between two tiles of fixed sizes
        Reduce(Reg[T](0))(tileSize by 1) { i => s1(i) * s2(i) } {
          _ + _
        }
      } {
        _ + _
      }
    }

    val gold = vec1.zip(vec2) {
      _ * _
    }.reduce {
      _ + _
    }

    assert(gold == getArg(x), r"Expected ${gold}, got ${getArg(x)}!")
  }
}

@spatial object PipelinedInnerProduct extends SpatialApp {

  def main(args: Array[String]): Unit = {
    type T = FixPt[TRUE, _24, _8]

    val tileSize = 64 // known while compiling

    val len = ArgIn[Int]
    setArg[Int](len, args(0).to[Int])

    val vec1 = Array.tabulate[T](len) { i => i.to[T] }
    val vec2 = Array.tabulate[T](len) { i => (len - i).to[T] }

    // DRAM to store input data
    val d1 = DRAM[T](len)
    val d2 = DRAM[T](len)

    setMem(d1, vec1)
    setMem(d2, vec2)

    val x = ArgOut[T]

    Accel {
      // These two SRAMs will be double-buffered automatically due to the enforced Pipe tag
      val s1 = SRAM[T](tileSize)
      val s2 = SRAM[T](tileSize)

      x := Pipe.Reduce(Reg[T](0))(tileSize by 1) { tile =>
        s1 load d1(tile :: tile + tileSize)
        s2 load d2(tile :: tile + tileSize)

        Pipe.Reduce(Reg[T](0)
        )(tileSize by 1) { i =>
          s1(i) * s2(i)
        } {
          _ + _
        }
      } {
        _ + _
      }
    }

    val gold = vec1.zip(vec2) {
      _ * _
    }.reduce {
      _ + _
    }

    assert(gold == getArg(x), r"Expected ${gold}, got ${getArg(x)}!")
  }
}