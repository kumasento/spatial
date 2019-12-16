/**
  * Auto-diff demo on vector inputs.
  */

import spatial.dsl._

/**
  * Dot-product auto-differentiation.
  *
  * f(v1, v2) = sum v1_i * v2_i
  */
@spatial object DotProdAutoDiff extends SpatialApp {
  def main(args: Array[String]): Unit = {
    type T = FixPt[TRUE, _24, _8] // type alias

    // test data
    val N = 16
    val v1_ = Array.tabulate[T](N) { i => i.to[T] }
    val v2_ = Array.tabulate[T](N) { i => (N.to[Int] - i).to[T] }

    // memory
    val v1 = DRAM[T](N)
    val v2 = DRAM[T](N)
    setMem(v1, v1_)
    setMem(v2, v2_)

    val z = ArgOut[T] // result
    // gradients
    val dzdv1 = DRAM[T](N)
    val dzdv2 = DRAM[T](N)

    Accel {
      val s1 = SRAM[T](N)
      val s2 = SRAM[T](N)

      s1 load v1
      s2 load v2

      val z_ = Reduce(Reg[T](0))(N by 1) { i => s1(i) * s2(i) } {
        _ + _
      }

      // CORE auto-diff logic
      // TODO: is it necessary to do type casting here?
      val dzds1 = getGradSRAM(z_, s1).asInstanceOf[SRAM1[T]]
      val dzds2 = getGradSRAM(z_, s2).asInstanceOf[SRAM1[T]]

      // store back results
      dzdv1 store dzds1
      dzdv2 store dzds2
      z := z_
    }

    println(r"z = $z")
    println(r"dzdv1 = ${getArray(dzdv1)}")
    println(r"dzdv1 = ${getArray(dzdv2)}")

  }
}
