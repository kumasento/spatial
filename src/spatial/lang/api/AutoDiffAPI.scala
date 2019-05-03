/**
  * Constructs for auto-diff.
  */

package spatial.lang.api

import forge.tags._
import argon._
import spatial.node._

trait AutoDiffAPI { this: Implicits =>

  @api def getGrad[A](a: Bits[A], b: Bits[A]): A = {
    implicit val tA: Bits[A] = a.selfType
    stage(Grad[A](a, b))
  }
}
