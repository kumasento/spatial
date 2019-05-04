import spatial.dsl._

@spatial object PrimTest extends SpatialApp {
  def main(args: Array[String]): Unit = {
    val x = ArgIn[Int]
    val y = ArgIn[Int]
    val z = ArgOut[Int]
    val t = ArgOut[Int]
    val d = ArgOut[Int]

    setArg(x, 2)
    setArg(y, 3)

    Accel {
      t := prim(x, y)
      d := x + y
      z := prim(x, y)
    }

    println(r"Sum of x and y is d = ${getArg(d)}")
    assert(getArg(t) == x * y, r"Expected t to be ${x * y}, got: ${getArg(t)}")
    assert(getArg(z) == x * x + y * y, r"Expected z to be ${x * x + y * y}, got: ${getArg(z)}")
  }
}

