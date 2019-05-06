/**
  * Auto-diff demo on scalar arithmetic.
  */
import spatial.dsl._

/**
  * The formula of this Spatial app is:
  *
  * y = f(x) = x**2 + x
  * dy/dx(x) = 2 x + 1
  */
@spatial object ScalarAutoDiffSingleInput extends SpatialApp {

  def main(args: Array[String]): Unit = {
    type T = FixPt[TRUE, _24, _8]

    // The input parameter
    val x = ArgIn[T]
    setArg(x, 2.0) // just use an arbitrary value.

    // The gradient output
    val dydx = ArgOut[T]

    Accel {
      // Perform the calculation
      val y = x ** 2 + x

      // Call the auto-diff API to compute the gradient
      dydx := getGrad[T](y, x)
    }

    println(r"x = ${getArg(x)}, dy/dx = ${getArg(dydx)}")
  }
}
