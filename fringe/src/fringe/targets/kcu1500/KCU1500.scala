package fringe.targets.kcu1500

import chisel3._
import fringe.BigIP
import fringe.{AbstractAccelUnit, BigIP, SpatialIPInterface}
import fringe.targets.DeviceTarget
import fringe.targets.zynq.FringeZynq

class KCU1500 extends DeviceTarget {

  def makeBigIP: BigIP = new fringe.targets.zynq.BigIPZynq

  override val magPipelineDepth: Int = 0
  override def regFileAddrWidth(n: Int): Int = 32

  override def addFringeAndCreateIP(reset: Reset, accel: AbstractAccelUnit): SpatialIPInterface = {
    val io = IO(new KCU1500IPInterface)

    // Zynq Fringe
    val blockingDRAMIssue = false // Allow only one in-flight request, block until response comes back
    val fringe = Module(new FringeZynq(blockingDRAMIssue, io.axiLiteParams, io.axiParams))

    // Fringe <-> Host connections
    fringe.io.S_AXI <> io.S_AXI

    // SpatialIP <-> AXIStream connections (note they go directly from accel to spatial IP)
    io.AXIS_IN <> accel.io.axiStreamsIn.head
    io.AXIS_OUT <> accel.io.axiStreamsOut.head

    // Fringe <-> DRAM connections
    io.M_AXI <> fringe.io.M_AXI

    fringe.io.TOP_AXI <> DontCare
    fringe.io.DWIDTH_AXI <> DontCare
    fringe.io.PROTOCOL_AXI <> DontCare
    fringe.io.CLOCKCONVERT_AXI <> DontCare

    // io.rdata handled by bridge inside FringeZynq
    io.rdata := DontCare

    accel.io.argIns := fringe.io.argIns
    fringe.io.argOuts.zip(accel.io.argOuts) foreach { case (fringeArgOut, accelArgOut) =>
      fringeArgOut.bits := accelArgOut.port.bits
      fringeArgOut.valid := accelArgOut.port.valid
    }
    fringe.io.argEchos.zip(accel.io.argOuts) foreach { case (fringeArgOut, accelArgOut) =>
      accelArgOut.echo := fringeArgOut
    }

    // accel.io.argIOIns := fringe.io.argIOIns
    // fringe.io.argIOOuts.zip(accel.io.argIOOuts) foreach { case (fringeArgOut, accelArgOut) =>
    //     fringeArgOut.bits := accelArgOut.bits
    //     fringeArgOut.valid := 1.U
    // }
    fringe.io.externalEnable := false.B
    fringe.io.memStreams <> accel.io.memStreams
    fringe.io.heap <> accel.io.heap
    accel.io.enable := fringe.io.enable
    fringe.io.done := accel.io.done
    fringe.reset := reset.toBool
    accel.reset := fringe.io.reset
//    accel.clock := io.ACCEL_CLK // TODO: Temporary hack where accel is wired to DMAClock of the Rogue wrapper to avoid having to use an AXI4ClkConverter for now
    // accel.reset := ~reset.toBool
    // io.is_enabled := ~accel.io.enable

    io
  }

  override val addrWidth: Int = 32
  override val dataWidth: Int = 32
  override val external_w: Int = 32
  override val external_v: Int = 16
  override val wordsPerStream: Int = 16
  override val num_channels = 1

}