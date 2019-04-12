package spatial.transform

import argon._
import argon.node._
import argon.transform.MutateTransformer
import spatial.node.AccelScope
import spatial.traversal.AccelTraversal

case class DummyTransformer(IR: State) extends MutateTransformer with AccelTraversal {

  override def transform[A: Type](lhs: Sym[A], rhs: Op[A])(implicit ctx: SrcCtx): Sym[A] = rhs match {
    case _ => super.transform(lhs, rhs)
  }
}
