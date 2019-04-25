/** The demo of forward and backward computation of a
  * 2-layers MLP. */

import spatial.dsl._

@spatial object MLPGradDemo extends SpatialApp {

  def main(args: Array[String]): Unit = {
    type T = FixPt[TRUE, _24, _8]

    // MLP configuration
    // 3 layers both have 512 hidden units, input size 784, outputs a
    // length-10 vector.
    val num_classes: Int = 10
    val in_size: Int = 784
    val num_units_l1: Int = 512
    val num_units_l2: Int = 512

    // Interface to the hardware
    val NUM_CLASSES = ArgIn[Int]
    val IN_SIZE = ArgIn[Int]
    // TODO: ArgIn with array input?
    val NUM_UNITS_L1 = ArgIn[Int]
    val NUM_UNITS_L2 = ArgIn[Int]

    setArg(NUM_CLASSES, num_classes)
    setArg(IN_SIZE, in_size)
    setArg(NUM_UNITS_L1, num_units_l1)
    setArg(NUM_UNITS_L2, num_units_l2)

    // Generate test data
    // input vector (vec_in) contains values in range [-1, 1]
    println("Generating test data ...")
    val vec_in = Array.tabulate[T](IN_SIZE) { _ => random[T](2) - 1 } // input

    // generate the one-hot label, set to 1 at a random position
    val vec_tg = Array[T](0, 1, 0, 0, 0, 0, 0, 0, 0, 0)

    // generate weights for each layer
    // TODO: don't manually write the generation code for all layers
    val mat_w1 = (0 :: NUM_UNITS_L1, 0 :: IN_SIZE) { (_, _) => random[T](128) - 64 }
    val mat_w2 = (0 :: NUM_UNITS_L2, 0 :: NUM_UNITS_L1) { (_, _) => random[T](128) - 64 }
    val mat_w3 = (0 :: NUM_CLASSES, 0 :: NUM_UNITS_L2) { (_, _) => random[T](128) - 64 }

    // Generate DRAM
    val d_vec_in = DRAM[T](IN_SIZE)
    val d_vec_tg = DRAM[T](NUM_CLASSES)
    val d_mat_w1 = DRAM[T](NUM_UNITS_L1, IN_SIZE)
    val d_mat_w2 = DRAM[T](NUM_UNITS_L2, NUM_UNITS_L1)
    val d_mat_w3 = DRAM[T](NUM_CLASSES, NUM_UNITS_L2)


    // Set up DRAM content
    println("Initializing DRAM contents ...")
    setMem(d_vec_in, vec_in)
    setMem(d_vec_tg, vec_tg)
    setMem(d_mat_w1, mat_w1)
    setMem(d_mat_w2, mat_w2)
    setMem(d_mat_w3, mat_w3)

    // Set up output - the final loss
    val loss = ArgOut[T]

    println("Running simulation ...")
    Accel {
      // Naive solution - create SRAM the same size as the DRAM.
      // All intermediate results are stored on chip.
      val s_vec_in = SRAM[T](in_size)
      val s_vec_tg = SRAM[T](num_classes)
      val s_vec_w1 = SRAM[T](in_size) // load one row each time
      val s_vec_w2 = SRAM[T](num_units_l1)
      val s_vec_w3 = SRAM[T](num_units_l2)
      // intermediate results
      val s_vec_l1 = SRAM[T](num_units_l1)
      val s_vec_l2 = SRAM[T](num_units_l2)
      val s_vec_sm = SRAM[T](num_classes) // softmax

      // load input vector
      s_vec_in load d_vec_in

      // layer 1
      Foreach(NUM_UNITS_L1 by 1) { i =>
        s_vec_w1 load d_mat_w1(i, 0 :: IN_SIZE)
        s_vec_l1(i) = Reduce(Reg[T](0))(IN_SIZE by 1) { i => s_vec_w1(i) * s_vec_in(i) } {
          _ + _
        }
        s_vec_l1(i) = mux[T](s_vec_l1(i) < 0, 0.to[T], s_vec_l1(i))
      }

      // layer 2
      Foreach(NUM_UNITS_L2 by 1) { i =>
        s_vec_w2 load d_mat_w2(i, 0 :: NUM_UNITS_L1)
        s_vec_l2(i) = Reduce(Reg[T](0))(NUM_UNITS_L1 by 1) { i => s_vec_w2(i) * s_vec_l1(i) } {
          _ + _
        }
        s_vec_l2(i) = mux[T](s_vec_l2(i) < 0, 0.to[T], s_vec_l2(i))
      }

      // layer 3
      Foreach(NUM_CLASSES by 1) { i =>
        s_vec_w3 load d_mat_w3(i, 0 :: NUM_UNITS_L2)
        s_vec_sm(i) = Reduce(Reg[T](0))(NUM_UNITS_L2 by 1) { i => s_vec_w3(i) * s_vec_l2(i) } {
          _ + _
        }
        s_vec_sm(i) = mux[T](s_vec_l2(i) < 0, 0.to[T], s_vec_l2(i))
        // softmax
        s_vec_sm(i) = exp[T](s_vec_sm(i))
      }

      // softmax
      val sum = Reduce(Reg[T](0))(NUM_CLASSES by 1) { i => s_vec_sm(i) } {
        _ + _
      }
      Foreach(NUM_CLASSES by 1) { i => s_vec_sm(i) = s_vec_sm(i) / sum }

      // cross-entropy loss
      s_vec_tg load d_vec_tg
      loss := Reduce(Reg[T](0))(NUM_CLASSES by 1) { i =>
        -s_vec_tg(i) * log_taylor[T](s_vec_sm(i))
      } {
        _ + _
      }
    }

    println("Loss value = " + loss)
  }
}