package spatial.transform

import argon._
import argon.transform.MutateTransformer
import spatial.traversal.AccelTraversal

/** Implements the auto-differentiation algorithm.
  *
  * @param IR
  */
case class AutoDiffTransformer(IR: State) extends MutateTransformer with AccelTraversal {

  /** Transformation of the IR by auto diff.
    *
    * @param lhs the symbol of the current entity (ID)
    * @param rhs the operation (node)
    * @param ctx where the statement is within a source code file.
    * @tparam A
    * @return the symbol which should replace lhs
    */
  override def transform[A:Type](lhs: Sym[A], rhs: Op[A])(implicit ctx: SrcCtx): Sym[A] = rhs match {
    case _ => {
      println("lhs = " + lhs + " rhs = " + rhs + " ctx = " + ctx)
      super.transform(lhs, rhs)
    }
  }
}
