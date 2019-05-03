package spatial.transform

import argon._
import argon.node._
import argon.transform.MutateTransformer
import spatial.node._
import spatial.lang._
import spatial.traversal.AccelTraversal

case class PrimTransformer(IR: State) extends MutateTransformer {

  var added: Set[(Sym[_], Sym[_])] = Set.empty

  def getPrimReplacement[A: Type, S, I, F](a: Fix[S, I, F], b: Fix[S, I, F]): Fix[S, I, F] = {
    implicit val S: BOOL[S] = a.fmt.s
    implicit val I: INT[I] = a.fmt.i
    implicit val F: INT[F] = a.fmt.f

    (a, b) match {
      case (Op(RegRead(ra)), Op(RegRead(rb))) =>
        println(s"Current set is $added, required symbol: $ra, $rb")

        if (added.contains((ra, rb)))
          stage(FixAdd(stage(FixMul(a, a)), stage(FixMul(b, b))))
        else
          stage(FixMul(a, b))
      case _ =>
        stage(FixAdd(a, b))
    }

  }

  override def transform[A: Type](lhs: Sym[A], rhs: Op[A])(implicit ctx: SrcCtx): Sym[A] =
    rhs match {
      case FixAdd(F(Op(RegRead(a))), F(Op(RegRead(b)))) => {
        println(s"lhs = $lhs, rhs = $rhs")

        println(s"Meet $a and $b")
        added += ((a, b))
        super.transform(lhs, rhs)
      }

      case Prim(F(a: Fix[_, _, _]), F(b: Fix[_, _, _])) => {
        println(s"lhs = $lhs, rhs = $rhs")

        val lhs2 = getPrimReplacement(a, b)
        println(s"Matched prim, replaced with $lhs2")
        lhs2.asInstanceOf[Sym[A]]
      }

      case _ => {
        println(s"lhs = $lhs, rhs = $rhs")
        super.transform(lhs, rhs)
      }
    }
}
