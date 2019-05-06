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

  def insertReverseADFix[S, I, F](y: Fix[S, I, F], x: Fix[S, I, F]): Fix[S, I, F] = {
    println(s"Inserting gradient for $y over $x ...")
    println(s"$y inputs = ${y.inputs} op = ${y.op}")

    type T = Fix[S, I, F]

    implicit val S: BOOL[S] = x.fmt.s
    implicit val I: INT[I] = x.fmt.i
    implicit val F: INT[F] = x.fmt.f

    if (x == y) {
      1.0.to[T]
    } else if (y.inputs.isEmpty) {
      0.0.to[T]
    } else {
      y.op match {
        case Some(FixMul(v1, v2)) =>
          val dv1dx = insertReverseADFix(v1, x)
          val dv2dx = insertReverseADFix(v2, x)
          stage(FixAdd(stage(FixMul(v1, dv2dx)), stage(FixMul(v2, dv1dx))))

        // TODO: how to avoid duplicating similar logic?
        case Some(FixAdd(v1, v2)) =>
          val dv1dx = insertReverseADFix(v1, x)
          val dv2dx = insertReverseADFix(v2, x)
          stage(FixAdd(dv1dx, dv2dx))

        case Some(FixSub(v1, v2)) =>
          val dv1dx = insertReverseADFix(v1, x)
          val dv2dx = insertReverseADFix(v2, x)
          stage(FixSub(dv1dx, dv2dx))

        case Some(RegRead(v)) => // TODO: is this correct?
          if (v.asSym == x.asSym) 1.0.to[T] else 0.0.to[T]

        case None => // TODO: is this always a Const?
          0.0.to[T]

        case _ => throw new RuntimeException(s"Unrecognized op: ${y.op}")
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
    case Grad(F(y: Fix[_, _, _]), F(x: Fix[_, _, _])) => {
      println(s"Found Grad: $y over $x")
      insertReverseADFix(y, x).asInstanceOf[Sym[A]]
    }
    case _ => super.transform(lhs, rhs)
  }
}
