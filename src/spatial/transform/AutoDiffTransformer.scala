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
    // TODO: should deprecate this function
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

        // TODO: is this correct? Should find a way to validate
        case Some(RegRead(v)) =>
          if (v.asSym == x.asSym) 1.0.to[T] else 0.0.to[T]

        case None => // TODO: is this always a Const?
          0.0.to[T]

        case _ => throw new RuntimeException(s"Unrecognized op: ${y.op}")
      }
    }
  }

  def findCtrlOp[A](x: Sym[A]): Option[Sym[_]] = {
    x.consumers.find(p = p => {
      println(s"p = $p op = ${p.op}")
      p.op match {
        case Some(OpReduce(_, _, _, _, _, _, _, _, _, _, _)) => true
        case _ => false
      }
    })
  }

  def getBlockInputSymbols[A](blk: Block[A], unbound: Boolean): Seq[Sym[_]] = {
    // TODO: should I use Seq[Sym[A]] here? how to resolve the comfort problem?
    blk.stms.flatMap {
      _.inputs.filter {
        if (unbound) _.op.isEmpty else _.op.isDefined
      }
    }.distinct.filter {
      !blk.stms.contains(_)
    }
  }

  def getBlockRevAD[A](blk: Block[A]): Seq[Block[_]] = {
    println(s"==> Getting the derivatives within a block: $blk ...")
    println(s"Block inputs: ${blk.inputs}")
    println(s"Statements:")
    blk.stms.foreach { stm =>
      println(s"\t$stm ${stm.op} ${stm.inputs}")
      println(s"\t-> Inputs:")
      stm.inputs.foreach { i => println(s"\t\tSym=$i Op=${i.op}") }
    }

    // collect all unbound inputs, which should be the addresses
    // TODO: there should be only one elem. how to generalize?
    val ubIns = getBlockInputSymbols(blk, unbound = true)
    println(s"Unbound inputs to this block: $ubIns")
    assert(ubIns.size == 1)

    val allIns = getBlockInputSymbols(blk, unbound = false)
    println(s"All inputs: $allIns")

    // block output is the result from the last statement -
    val lastStm = blk.stms.last
    println(s"The last stm: $lastStm ${lastStm.op}")

    // the derivatives
    println(s"Calculating the partial derivatives: $lastStm over $allIns")
    val grads = allIns.map { in =>
      println(s"-> Creating grad for input $in ..")
      val grad = stageBlock {
        revAD(lastStm, in, Some(ubIns.head))
      }
      println(grad.stms)
      grad
    }
    println(s"grads = $grads")

    grads
  }

  def regReadAD[A](v: Reg[A], x: Sym[A]): Sym[A] = {
    implicit val tA: Bits[A] = v.A

    val ret = if (v.asSym == x) v.A.one else v.A.zero
    tA.box(ret).asInstanceOf[Sym[A]]
  }

  def fixMulAD[A, S, I, F](y: Sym[A], x: Sym[A], v1: Fix[S, I, F], v2: Fix[S, I, F], p: Option[Sym[A]] = None): Sym[A] = {
    type T = Fix[S, I, F]
    implicit val S: BOOL[S] = v1.fmt.s
    implicit val I: INT[I] = v1.fmt.i
    implicit val F: INT[F] = v1.fmt.f

    println(s"---> Creating fixMulAD: y = $y x = $x v1 = $v1 v2 = $v2 p = $p ...\n")

    val dv1dx = revAD(v1, x, p).asInstanceOf[T]
    val dv2dx = revAD(v2, x, p).asInstanceOf[T]
    stage(FixAdd(stage(FixMul(v1, dv2dx)), stage(FixMul(v2, dv1dx)))).asInstanceOf[Sym[A]]
  }

  def SRAMReadAD[A](x: Sym[A], p: Option[Sym[A]], op: SRAMRead[A, Any]): Sym[A] = {
    println(s"---> Creating SRAMReadAD: x = $x p = $p op = $op\n")

    assert(p.isDefined)
    val ret = if (op.mem == x && p.get == op.addr.head) op.A.one else op.A.zero
    implicit val tA: Bits[A] = op.A
    tA.box(ret).asInstanceOf[Sym[A]]
  }

  def revAD[A](y: Sym[A], x: Sym[A], p: Option[Sym[A]] = None): Sym[A] = {
    println(s"===> Running revAD:")
    println(s"\ty = $y y.op = ${y.op} x = $x x.op = ${x.op}")
    println(s"\ty.inputs = ${y.inputs}")
    println(s"\ty.consumers = ${y.consumers}")
    println(s"\ty.effects = ${y.effects}")
    println("")

    y.op match {
      // basic arithmetic
      case Some(FixMul(v1: Fix[_, _, _], v2: Fix[_, _, _])) =>
        fixMulAD(y, x, v1, v2, p)

      // memory operations
      case Some(op@SRAMRead(v1, p1, _)) =>
        // TODO: how could we create a proper constant?
        // TODO: is op.A the type to be returned?
        SRAMReadAD(x, p, op)

      case Some(RegRead(z)) =>
        revAD(z, x).asInstanceOf[Sym[A]]
      case Some(RegNew(_)) =>
        findCtrlOp(y) match {
          case Some(ctrl) =>
            ctrl.op match {
              case Some(op@OpReduce(_, _, accum, map, load, reduce, _, _, _, fold, _)) =>
                println("Needs to handle OpReduce")
                println(s"accum = $accum map = $map")
                println(s"A=${op.A}")
                map.stms.foreach { stm =>
                  println(s"MAP $stm ${stm.op} ${stm.inputs}")
                  stm.inputs.foreach { i => println(s"$i ${i.op}") }
                }
                getBlockRevAD(map)

                reduce.stms.foreach { stm =>
                  println(s"RED $stm ${stm.op} ${stm.inputs}")
                  stm.inputs.foreach { i => println(s"$i ${i.op}") }
                }

                // TODO: an ad-hoc solution
                if (reduce.stms.size == 1) {
                  reduce.stms.head.op match {
                    case Some(FixAdd(b1, b2)) =>
                      throw new RuntimeException(s"$b1, $b2")
                    case _ => throw new RuntimeException
                  }
                } else {
                  throw new RuntimeException(s"Unrecognized control op: ${ctrl.op}")
                }
              case _ =>
                throw new RuntimeException(s"Unrecognized control op: ${ctrl.op}")
            }
          case _ => throw new RuntimeException
        }
      case _ => throw new RuntimeException(s"Unrecognized op: ${y.op}")
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
  override def transform[A: Type](lhs: Sym[A], rhs: Op[A])(
    implicit ctx: SrcCtx): Sym[A] = rhs match {
    case Grad(F(y: Fix[_, _, _]), F(x: Fix[_, _, _])) =>
      println(s"Found Grad: $y over $x")
      insertReverseADFix(y, x).asInstanceOf[Sym[A]]

    case op@GradSRAM(_, F(y: Bits[A]), F(x: SRAM[A, _])) => // TODO: no idea why
      println(s"Found GradSRAM op = $op inputs = ${op.inputs}")

      revAD(y, x.asInstanceOf[Sym[A]])

    // super.transform(lhs, rhs)
    case _ =>
      // println(s"lhs = $lhs rhs = $rhs")
      super.transform(lhs, rhs)
  }
}
