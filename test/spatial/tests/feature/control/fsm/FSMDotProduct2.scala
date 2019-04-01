package spatial.tests.feature.control.fsm

import spatial.dsl._

@spatial class FSMDotProduct2 extends SpatialTest {
  override def dseModelArgs: Args = "8"
  override def finalModelArgs: Args = "8"

  def main(args: Array[String]): Unit = {
    val vectorA = Array.fill(128) {
      random[Int](10)
    }
    val vectorB = Array.fill(128) {
      random[Int](10)
    }
    val vecA = DRAM[Int](128)
    val vecB = DRAM[Int](128)
    val out = ArgOut[Int]
    setMem(vecA, vectorA)
    setMem(vecB, vectorB)
    Accel {
      val outer_accum = Reg[Int](0)
      FSM(0)(i => i < 128) { i =>
        val a = SRAM[Int](16)
        val b = SRAM[Int](16)
        Parallel {
          a load vecA(i :: i + 16 par 16)
          b load vecB(i :: i + 16 par 16)
        }
        outer_accum := outer_accum + Reduce(0)(0 until 16) { i => a(i) * b(i) } {
          _ + _
        }
      } { i => i + 16 }
      Pipe{out := outer_accum}
    }
    val result = getArg(out)
    val gold = vectorA.zip(vectorB){_ * _}.reduce {_ + _}
    println("Expected: " + gold + ", got: " + result)
    // assert(result == gold, "Result (" + result + ") did not equal expected (" + gold + ")")
    assert(result == gold)
  }
}
