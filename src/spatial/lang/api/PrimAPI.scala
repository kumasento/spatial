package spatial.lang.api

import argon._
import forge.tags._

import spatial.node.Prim

trait PrimAPI { this: Implicits =>
  @api def prim[A](a: Bits[A], b: Bits[A]): A = {
    implicit val tA: Bits[A] = a.selfType
    stage(Prim(a, b))
  }
}
