package spatial.node

import argon._
import argon.node._
import forge.tags._
import spatial.lang._

@op case class Prim[A:Bits](a: Bits[A], b: Bits[A]) extends Primitive[A] {}
