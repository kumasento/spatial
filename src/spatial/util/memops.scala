package spatial.util

import argon._
import forge.tags._

import spatial.lang._
import spatial.node._
import spatial.metadata.memory._

import argon.node._
import spatial.metadata.control._
import utils.tags.instrument

import utils.implicits.collections._

object memops {

  implicit class AliasOps[A](mem: Sym[A]) {
    @rig def sparseStarts(): Map[Int,I32] = {
      if (mem.isSparseAlias) throw new Exception(s"Cannot get starts of sparse alias")
      mem.sparseRank.map{ d => d -> stage(MemStart(mem, d)) }.toMap
    }
    @rig def sparseSteps(): Map[Int,I32] = {
      if (mem.isSparseAlias) throw new Exception(s"Cannot get steps of sparse alias")
      mem.sparseRank.map{ d => d -> stage(MemStep(mem, d)) }.toMap
    }
    @rig def sparseEnds(): Map[Int,I32] = {
      if (mem.isSparseAlias) throw new Exception(s"Cannot get ends of sparse alias")
      mem.sparseRank.map{ d => d -> stage(MemEnd(mem, d)) }.toMap
    }
    @rig def sparsePars(): Map[Int,I32] = {
      if (mem.isSparseAlias) throw new Exception(s"Cannot get pars of sparse alias")
      mem.sparseRank.map{ d => d -> stage(MemPar(mem, d)) }.toMap
    }
    @rig def sparseLens(): Map[Int,I32] = {
      mem.sparseRank.map{ d => d -> stage(MemLen(mem, d)) }.toMap
    }
    @rig def sparseOrigins[W:INT](): Map[Int,Ind[W]] = {
      mem.sparseRank.map{ d => d -> stage(MemOrigin(mem, d)) }.toMap
    }

    @rig def rawStarts(): Seq[I32] = {
      if (mem.isSparseAlias) throw new Exception(s"Cannot get rawStarts of sparse alias")
      mem.rawRank.map{d => stage(MemStart(mem, d)) }
    }

    @rig def rawDims(): Seq[I32] = {
      if (mem.isSparseAlias) throw new Exception(s"Cannot get rawDims of sparse alias")
      mem.rawRank.map{d => stage(MemDim(mem, d)) }
    }

    def parsImm: Seq[I32] = mem match {
      case Op(alias: MemDenseAlias[_,_,_]) => alias.ranges.map(_.head.par)
      case Op(alloc: MemAlloc[_,_])      => Seq.fill(alloc.rank.length){ I32(1) }
      case _ => Nil
    }

    @rig def rawSeries(): Seq[Series[I32]] = {
      if (mem.isSparseAlias) throw new Exception(s"Cannot get series of sparse alias")
      val sparseRank = mem.sparseRank
      mem.rawRank.map{
        case d if sparseRank.contains(d) =>
          val start = stage(MemStart(mem, d))
          val end   = stage(MemEnd(mem, d))
          val step  = stage(MemStep(mem, d))
          val par   = stage(MemPar(mem, d))
          Series(start, end, step, par)
        case _ =>
          Series(I32(0),I32(1),I32(1),I32(1))
      }
    }

    @rig def addrs[W:INT]() = mem match {
      case Op(op: MemSparseAlias[_,_,_,_,_]) =>
        def addrAlias[Addr[T]](implicit Addr: Type[Addr[Ind[W]]]) = {
          val addr = op.addr.map{mem => mem.asInstanceOf[Addr[Ind[W]]]}

          if (addr.lengthMoreThan(1)) {
            val ranges = addr.map{mem => Addr.boxed(mem).rawSeries() }
            stage(MemDenseAlias[Ind[W],Addr,Addr](op.cond,addr,ranges))
          }
          else addr.head
        }
        addrAlias(op.Addr).asInstanceOf[LocalMem[Ind[W],C forSome{type C[_]}]]

      case _ => throw new Exception(s"No sparse addresses available for $mem")
    }

    // TODO[2]: Units
    //def units(): Seq[Boolean] = metadata[AliasUnit](x).get.unit()
  }

}
