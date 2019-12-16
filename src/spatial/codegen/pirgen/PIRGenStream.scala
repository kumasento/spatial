package spatial.codegen.pirgen

import argon._
import spatial.lang._
import spatial.node._
import spatial.metadata.memory._

trait PIRGenStream extends PIRCodegen {

  override protected def genAccel(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case op@StreamInNew(bus)  =>
      stateMem(lhs, "FIFO()")
      val streams = mapStruct(lhs.asMem.A) { s => Lhs(lhs, s.map { _._1 })}
      emit(src"""streamIn($streams, $bus)""")

    case op@StreamOutNew(bus) =>
      val streams = mapStruct(lhs.asMem.A) { s => Lhs(lhs, s.map { _._1 })}
      stateMem(lhs, "FIFO()")
      emit(src"""streamOut($streams, $bus)""")

    case op@StreamInBankedRead(strm, ens) =>
      stateAccess(lhs, strm, ens) {
        src"MemRead()"
      }

    case StreamOutBankedWrite(strm, data, ens) =>
      stateAccess(lhs, strm, ens, data=Some(data)) {
        src"MemWrite()"
      }

    case _ => super.genAccel(lhs, rhs)
  }

  override protected def quoteOrRemap(arg: Any): String = arg match {
    case x:DRAMBus[_] => "DRAMBus"
    case x => super.quoteOrRemap(arg)
  }

}
