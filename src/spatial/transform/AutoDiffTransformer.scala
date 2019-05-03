package spatial.transform

import argon._
import argon.node._
import spatial.node._
import spatial.lang._
import argon.passes._
import spatial.metadata.control._
import spatial.metadata.types._
import argon.transform.MutateTransformer
import spatial.traversal._

/** Implements the auto-differentiation algorithm.
  *
  * @param IR
  */
case class AutoDiffTransformer(IR: State) extends MutateTransformer with BlkTraversal {

  private def insertReverseAutoDiff[R](block: Block[R], grad: Sym[_]): Block[R] = {
    stageBlock(f(block.inputs), block.options) { block.result }
  }

  /**
    * Transform if there is a request for getGrad
    *
    * @param lhs
    * @param rhs
    * @param ctx
    * @tparam A
    * @return
    */
  private def transformGrad[A: Type](lhs: Sym[A], rhs: Op[A])(implicit ctx: SrcCtx): Sym[A] = rhs match {
    case ctrl: Control[_] => {
      inCtrl(lhs) {
        // Only a Control Op is expected here
        println(s"lhs = $lhs rhs = $rhs")

        ctrl.bodies.foreach {
          body =>
            dbg(s"In ctrl body $body")

            body.blocks.foreach {
              case (_, blk) =>
                println(s"blk = $blk inputs = ${
                  blk.inputs
                }")

                blk.stms.reverse.foreach { stm =>
                  println(s"stm = $stm op = ${stm.op} inputs = ${stm.inputs}")
                  stm.op match {
                    case Some(Grad(y, x)) =>
                      register(blk -> insertReverseAutoDiff(blk, lhs))
                    case _ => {}
                  }
                }
            }
        }
        super.transform(lhs, rhs)
      }
    }
  }

  /** Transformation of the IR by auto diff
    *
    * The original visit sequence is kind of bypassed here.
    *
    * @param lhs the symbol of the current entity (ID)
    * @param rhs the operation (node)
    * @param ctx where the statement is within a source code file.
    * @tparam A
    * @return the symbol which should replace lhs
    */
  override def transform[A: Type](lhs: Sym[A], rhs: Op[A])(implicit ctx: SrcCtx): Sym[A] = rhs match {
    case _: AccelScope => inAccel {
      transformGrad(lhs, rhs)
    }
    case _ => super.transform(lhs, rhs)
  }
}
