package spatial.metadata

import argon._
import spatial.lang.I32
import forge.tags.stateful
import spatial.metadata.bounds._
import spatial.metadata.control._

package object params {

  implicit class ParamDomainOps(p: Sym[_]) {
    def getParamDomain: Option[(Int,Int,Int)] = metadata[ParamDomain](p).map{d => (d.min,d.step,d.max) }
    def paramDomain: (Int,Int,Int) = getParamDomain.getOrElse((1,1,1))
    def paramDomain_=(d: (Int,Int,Int)): Unit = metadata.add(p, ParamDomain(d._1,d._2,d._3))

    def getContention: Option[Int] = metadata[MemoryContention](p).map{d => d.contention }
    def contention: Int = getContention.getOrElse(0)
    def contention_=(d: Int): Unit = metadata.add(p, MemoryContention(d))

    def getIntValue: Option[Int] = if (p.getBound.isDefined) Some(p.bound.toInt) else None
    @stateful def intValue: Int = {Console.println(s"getting int value for $p in state $state = ${p.bound.toInt}");p.bound.toInt}
    @stateful def setIntValue(d: Int): Unit = p.bound = Expect(d)
    def intValue_=(d: Int): Unit = p.bound = Expect(d)

    def getSchedValue: Option[CtrlSchedule] = p.getRawSchedule
    @stateful def schedValue: CtrlSchedule = p.rawSchedule
    @stateful def setSchedValue(d: CtrlSchedule): Unit = p.rawSchedule = d
    def schedValue_=(d: CtrlSchedule): Unit = p.rawSchedule = d
  }

  object Parameter {
    def unapply(x: Sym[_]): Option[Sym[_]] = x match {
      case x if x.isParam => Some(x)
      case _ => None
    }
  }

}
