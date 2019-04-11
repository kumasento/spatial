import spatial.dsl._

@spatial object InnerProduct extends SpatialApp {

  // Void is an argon type
  def main(args: Array[String]): Void = {
    // define data type
    type T = FixPt[TRUE, _24, _8]

    // // set vector length
    // val len = 64.to[Int]

    // // generate data
    // val vec1 = (0::len) { (i) => i.to[T] }
    // val vec2 = (0::len) { (i) => (len - i).to[T] }

    // // declare DRAM blocks
    // val d1 = DRAM[T](len)
    // val d2 = DRAM[T](len)

    // // initialize memory
    // setMem(d1, vec1)
    // setMem(d2, vec2)

    // // allocate register
    // val x = ArgOut[T]
    // Accel {
    //   // create local SRAMs
    //   val s1 = SRAM[T](len)
    //   val s2 = SRAM[T](len)

    //   // transfer data
    //   s1 load d1
    //   s2 load d2

    //   // multiplication and reduction
    //   x := Reduce(Reg[T](0))(len by 1) { i =>
    //     s1(i) * s2(i)
    //   }{_ + _}
    // }

    // // compute golden result
    // val gold = vec1.zip(vec2){_*_}.reduce{_+_}

    // assert(gold == getArg(x), r"Expected ${gold}, got ${getArg(x)}!")
  }
}
