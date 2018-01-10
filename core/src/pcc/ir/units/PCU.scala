package pcc
package ir
package units

case class PCU(eid: Int) extends PU[PCU](eid) {
  override type I = Any // TODO

  override def fresh(id: Int): PCU = PCU(id)
  override def stagedClass: Class[PCU] = classOf[PCU]
}
object PCU {
  implicit val pcu: PCU = PCU(-1)


}

case class PCUAlloc(


) extends BoxAlloc[PCU] {
  def mirror(f:Tx) = stage(this) // TODO
}