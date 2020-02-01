import spatial.dsl._

@spatial object HelloSpatialArgInOut extends SpatialApp {
    def main(args: Array[String]): Void = {
        val x = ArgIn[Int]
        val y = ArgOut[Int]
        val z = HostIO[Int]

        setArg(x, args(0).to[Int])
        setArg(z, args(1).to[Int])

        Accel {
            y := x + z
        }

        println(r"Result is ${getArg(y)}")
    }
}

@spatial object HelloSpatialMemory extends SpatialApp {

    def main(args: Array[String]): Void = {

        val x = ArgIn[Int]
        val y = ArgIn[Int]

        setArg(x, args(0).to[Int])
        setArg(y, args(1).to[Int])


        Accel {
            val s = SRAM[Int](16, 32)
            val r = Reg[Int]

            Foreach(16 by 1, 32 by 1) { (i, j) =>
                s(i, j) = i + j
            }

            r := s(x, y)

            println(r"Value of SRAM at (${x.value}, ${y.value}) is ${r.value}")
        }
    }

}

@spatial object HelloSpatialDRAM extends SpatialApp {

    def main(args: Array[String]): Void = {
        val d = DRAM[Int](16)

        val data = Array.fill[Int](16)(0)
        setMem(d, data)
        
        Accel {
            val s = SRAM[Int](16)

            s load d(0::16)

            Foreach(16 by 1){i => s(i) = s(i) + 1 }

            d(0::16) store s
        }

        printArray(getMem(d), "Result: ")
    }
}

@spatial object InnerProductPlain extends SpatialApp {
  def main(args: Array[String]): Void = {
    type T = FixPt[TRUE, _24, _8]

    val len = 64

    val vec1 = Array.tabulate[T](len){ i => i.to[T] }
    val vec2 = Array.tabulate[T](len){ i => (len - i).to[T] }

    val d1 = DRAM[T](len)
    val d2 = DRAM[T](len)

    setMem(d1, vec1)
    setMem(d2, vec2)

    val x = ArgOut[T]

    Accel {
      val s1 = SRAM[T](len)
      val s2 = SRAM[T](len)

      s1 load d1
      s2 load d2

      x := Reduce(Reg[T](0))(len by 1) { i => 
        s1(i) * s2(i)
      }{ _ + _ }

    }

    val gold = vec1.zip(vec2){ _ * _ }.reduce{ _ + _ }

    assert(gold == getArg(x), r"Expected ${gold}, got ${getArg(x)}")

    println(r"Result: ${getArg(x)}")
  }
}

@spatial object InnerProductTiled extends SpatialApp {
  def main(args: Array[String]): Void = {
    type T = FixPt[TRUE, _24, _8]

    val tileSize = 64

    val len = ArgIn[Int]
    setArg(len, args(0).to[Int])

    val vec1 = Array.tabulate[T](len) { i => i.to[T] }
    val vec2 = Array.tabulate[T](len) { i => (len - i).to[T] }
    val d1 = DRAM[T](len)
    val d2 = DRAM[T](len)
    setMem(d1, vec1)
    setMem(d2, vec2)

    val x = ArgOut[T]
    Accel {
      val s1 = SRAM[T](tileSize)
      val s2 = SRAM[T](tileSize)

      x := Reduce(Reg[T](0))(len by tileSize) { tile => 
        s1 load d1(tile::tile+tileSize)
        s2 load d2(tile::tile+tileSize)
        
        // will be returned directly
        Reduce(Reg[T](0))(tileSize by 1) { i => 
          mux(i + tile <= len, s1(i) * s2(i), 0.to[T])
        }{ _ + _ }
      }{ _ + _ }
    }

    val gold = vec1.zip(vec2){_ * _}.reduce{_ + _}

    assert(gold == getArg(x), r"Expected ${gold} got ${getArg(x)}!")

    println(r"Result = ${getArg(x)}")
  }
}

@spatial object InnerProductPipeline extends SpatialApp {
  def main(args: Array[String]): Void = {
    type T = FixPt[TRUE, _24, _8]

    val tileSize = 64

    val len = ArgIn[Int]
    setArg(len, args(0).to[Int])

    val vec1 = Array.tabulate[T](len) { i => i.to[T] }
    val vec2 = Array.tabulate[T](len) { i => (len - i).to[T] }
    val d1 = DRAM[T](len)
    val d2 = DRAM[T](len)
    setMem(d1, vec1)
    setMem(d2, vec2)

    val x = ArgOut[T]

    Accel {
      val s1 = SRAM[T](tileSize)
      val s2 = SRAM[T](tileSize)

      x := Pipe.Reduce(Reg[T](0))(len by tileSize) { tile => 
        s1 load d1(tile::tile+tileSize)
        s2 load d2(tile::tile+tileSize)

        Pipe.Reduce(Reg[T](0))(tileSize by 1) { i => 
          mux(i + tile <= len, s1(i) * s2(i), 0.to[T])
        }{ _ + _ }
      }{ _ + _ }
    }

    val gold = vec1.zip(vec2){_ * _}.reduce{_ + _}

    assert(gold == getArg(x), r"Expected ${gold}, got ${getArg(x)}!")

    println(r"Result = ${getArg(x)}")
  }
}

@spatial object InnerProductParallel extends SpatialApp {
  def main(args: Array[String]): Void = {
    type T = FixPt[TRUE, _24, _8]

    val tileSize = 64

    val len = ArgIn[Int]
    setArg(len, args(0).to[Int])

    val vec1 = Array.tabulate[T](len) { i => i.to[T] }
    val vec2 = Array.tabulate[T](len) { i => (len - i).to[T] }
    val d1 = DRAM[T](len)
    val d2 = DRAM[T](len)
    setMem(d1, vec1)
    setMem(d2, vec2)

    val x = ArgOut[T]
    Accel {
      val s1 = SRAM[T](tileSize)
      val s2 = SRAM[T](tileSize)

      x := Reduce(Reg[T](0))(len by tileSize par 2) { tile => 
        s1 load d1(tile::tile+tileSize)
        s2 load d2(tile::tile+tileSize)
        
        // will be returned directly
        Reduce(Reg[T](0))(tileSize by 1 par 4) { i => 
          mux(i + tile <= len, s1(i) * s2(i), 0.to[T])
        }{ _ + _ }
      }{ _ + _ }
    }

    val gold = vec1.zip(vec2){_ * _}.reduce{_ + _}

    assert(gold == getArg(x), r"Expected ${gold} got ${getArg(x)}!")

    println(r"Result = ${getArg(x)}")
  }
}
