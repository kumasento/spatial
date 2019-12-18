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