/**
  * Constructs for auto-diff.
  */

package spatial.lang.api

import forge.tags._
import argon._
import spatial.node._

trait AutoDiffAPI {
  this: Implicits =>

  @api def getGrad[A](a: Bits[A], b: Bits[A]): A = {
    implicit val tA: Bits[A] = a.selfType
    stage(Grad[A](a, b))
  }

  @api def getGradSRAM[A, C[T]](a: Bits[A], m: SRAM[A, C])(implicit tp: Type[C[A]]): C[A] = {
    // TODO: how to extend to multiple dimensions?
    implicit val tA: Bits[A] = a.selfType
    stage(GradSRAM(m.dims, a, m))
  }
}
