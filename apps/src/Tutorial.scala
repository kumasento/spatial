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