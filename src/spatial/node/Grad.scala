package spatial.node

import argon._
import argon.node._
import forge.tags._
import spatial.lang._
import utils.implicits.collections._

@op case class Grad[A:Bits](a: Bits[A], b: Bits[A]) extends Primitive[A] {

}
