package spatial.lang

import argon._
import emul.FixedPoint
import forge.tags._
import utils.implicits.collections._

import scala.annotation.unchecked.{uncheckedVariance => uV}

case class Series[+A:Num](
    start: A,
    end:   A,
    step:  A,
    par:   I32,
    isUnit: Boolean = false)
  extends Mirrorable[Series[_]] {

  def tp: Num[A@uV] = Num[A]

  def ::(start2: A@uV): Series[A] = Series[A](start2, end, start, par, isUnit=false)

  def par(p: I32): Series[A] = Series[A](start, end, step, p, isUnit=false)

  @api def length: A = (end - start + step - Num[A].from(1))/step

  @api def meta: Range = (start,end,step,par) match {
    case (Literal(s:Int),Literal(e:Int),Literal(stride:Int),_) => Range(s,e,stride)
    case _ =>
      val s = if (!start.isConst) "start" else ""
      val e = if (!end.isConst) "end" else ""
      val t = if (!step.isConst) "step" else ""
      val err = Seq(s,e,t).filter(_ != "")
      val xs  = err.mkString(" and ")
      val wrng = if (err.lengthMoreThan(1)) s"$xs are not constants" else s"$xs is not a constant"
      error(ctx, s"Cannot create metaprogrammed range: $wrng")
      error(ctx)
      Range(0, 0, 1)
  }

  def mirror(f:Tx): Series[_] = Series[A](f(start),f(end),f(step),f(par),isUnit)
  override def toString: String = s"Series($start, $end, $step, $par, $isUnit)"
}