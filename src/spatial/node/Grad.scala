package spatial.node

import argon._
import argon.node._
import forge.tags._
import spatial.lang._
import utils.implicits.collections._

@op case class Grad[A: Bits](a: Bits[A], b: Bits[A]) extends Primitive[A] {}

@op case class GradSRAM[A: Bits, C[T]](dims: Seq[I32], a: Bits[A], m: SRAM[A, C])
                                      (implicit val tp: Type[C[A]])
  extends MemAlloc[A, C] {}