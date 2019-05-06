/**
  * Auto-diff demo on scalar formulas.
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
    val y = ArgOut[T]
    val dydx = ArgOut[T]

    Accel {
      // Perform the calculation
      val y_ = x ** 2 + x

      // Call the auto-diff API to compute the gradient
      dydx := getGrad[T](y_, x)
      y := y_
    }

    println(r"x = ${getArg(x)}, y = ${getArg(y)} dy/dx = ${getArg(dydx)}")
  }
}

/**
  * Rosenbrock function
  * f(x, y) = (a - x)**2 + b (y - x**2) ** 2
  */
@spatial object ScalarAutoDiffMultiInputs extends SpatialApp {

  def main(args: Array[String]): Unit = {
    type T = FixPt[TRUE, _24, _8]

    val x = ArgIn[T]
    val y = ArgIn[T]
    val a = 2.0.to[T]
    val b = 3.0.to[T]
    val x_ = 1.0.to[T]
    val y_ = -1.0.to[T]

    setArg(x, x_)
    setArg(y, y_)

    val z = ArgOut[T]
    val dzdx = ArgOut[T]
    val dzdy = ArgOut[T]

    Accel {
      val z1 = (a - x) ** 2
      val z2 = b * ((y - x ** 2) ** 2)
      val z_ = z1 + z2

      dzdx := getGrad(z_, x)
      dzdy := getGrad(z_, y)

      z := z_
    }

    // golden derivatives
    val dzdx_ = -2.to[T] * a + 2.to[T] * x_ - 4.to[T] * b * y_ * x_ + 4.to[T] * b * (x_ ** 3)
    val dzdy_ = 2.to[T] * b * (y_ - x_ ** 2)
    assert(dzdx_ == getArg(dzdx), r"dz/dx is wrong, golden is $dzdx_, got ${getArg(dzdx)}")
    assert(dzdy_ == getArg(dzdy), r"dz/dy is wrong, golden is $dzdy_, got ${getArg(dzdy)}")

    println(r"z = ${getArg(z)} dz/dx = ${getArg(dzdx)} dz/dy = ${getArg(dzdy)}")
  }
}