package pcc.lang

import forge._
import pcc.core._
import pcc.node._
import pcc.node.pir.CounterChainCopy

/** Types **/
case class CounterChain(eid: Int) extends Sym[CounterChain](eid) {
  override type I = Array[Range]

  override def fresh(id: Int): CounterChain = CounterChain(id)
  override def stagedClass: Class[CounterChain] = classOf[CounterChain]
  override def isPrimitive: Boolean = false
}
object CounterChain {
  implicit val tp: CounterChain = CounterChain(-1)
  @api def apply(ctrs: Counter*): CounterChain = stage(CounterChainNew(ctrs))

  @api def copy(ctrs: Counter*): CounterChain = stage(CounterChainCopy(ctrs))
}


