package argon.passes

import argon._

case class IRPrinter(IR: State, enable: Boolean) extends Traversal {
  override val name = "IR"

  override def shouldRun: Boolean = enable

  override protected def postprocess[R](block: Block[R]): Block[R] = {
    dbgs("")
    dbgs(s"Global Metadata")
    dbgs(s"---------------")
    globals.foreach { (k, v) => dbgs(s"$k: $v") }
    super.postprocess(block)
  }

  private def printBlocks(lhs: Sym[_], blocks: Seq[Block[_]]): Unit = blocks.zipWithIndex.foreach { case (blk, i) =>
    state.logTab += 1
    dbgs(s"block $i: $blk {")
    state.logTab += 1
    dbgs(s"effects:  ${blk.effects}")
    visitBlock(blk)
    state.logTab -= 1
    dbgs(s"} // End of $lhs block #$i")
    state.logTab -= 1
  }

  override protected def visit[A](lhs: Sym[A], rhs: Op[A]): Unit = {
    if (rhs.blocks.nonEmpty) dbgs(s"$lhs = $rhs {") else dbgs(s"$lhs = $rhs")
    strMeta(lhs)

    if (rhs.binds.nonEmpty) {
      dbgs(s"binds: ")
      state.logTab += 1
      rhs.binds.filter(_.isBound).foreach { b =>
        dbgs(s"$b")
        strMeta(b)
      }
      state.logTab -= 1
    }

    printBlocks(lhs, rhs.blocks)

    if (rhs.blocks.nonEmpty) dbgs(s"} // End of $lhs")
  }
}
