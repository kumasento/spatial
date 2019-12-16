package spatial.model

import argon._
import argon.codegen.FileDependencies
import spatial.node._
import spatial.lang._
import spatial.metadata.params._
import spatial.metadata.control._
import spatial.metadata.memory._
import spatial.metadata.bounds._
import spatial.util.modeling._

trait ControlModels { this: RuntimeModelGenerator =>
  import spatial.dsl._

}

case class RuntimeModelGenerator(IR: State, version: String) extends FileDependencies with ControlModels {
  override val ext: String = ".scala"
  override val lang: String = "model"
  override val entryFile: String = s"model_$version.scala"

  var inCycle: Boolean = false
  var undefinedSyms: Set[Sym[_]] = Set.empty



  def getCtx(lhs: Sym[_]): String = s"${lhs.ctx.content.map(_.trim).getOrElse("(Inserted by compiler)")}"


  def isTuneable(s: Sym[_]): Boolean = {
    TileSizes.all.contains(s) ||
    ParParams.all.contains(s) ||
    PipelineParams.all.contains(s)
  }


  override def quote(s: Sym[_]): String = s.rhs match {
    case Def.TypeRef  => super.quote(s)
    // case Def.Const(x) if (isTuneable(s)) => s"""Tuneable(${s.hashCode}, $x, "$s")"""
    // case Def.Node(id,op) if (isTuneable(s)) => // TODO
    //   if (s.isInstanceOf[Fix[_,_,_]]) undefinedSyms += s
      s"""Tuneable("${s.name.get}", ${super.quote(s)})"""
    // case Def.Param(id, _) if (isTuneable(s)) => // TODO
    //   if (s.isInstanceOf[Fix[_,_,_]]) undefinedSyms += s
    //   s"Tuneable(${s.hashCode}, p$id)"
    case Def.Const(x) => s"$x"
    case Def.Node(_,_) =>
      if (s.isInstanceOf[Fix[_,_,_]]) undefinedSyms += s
      super.quote(s)
    case Def.Param(id, _) =>
      if (s.isInstanceOf[Fix[_,_,_]]) undefinedSyms += s
      s"p$id"
    case Def.Bound(_) => super.quote(s)
    case Def.Error(_,_) => super.quote(s)
  }

  def ctrHead(lhs: String, cchain: CounterChain): Unit = {
    cchain.counters.zipWithIndex.foreach { case (counter, i) =>
      emit(src"// Parallelization of counter #$i")
      emit(src"${lhs}_counter${i}_P = ${counter.ctrPar}")
    }
  }

  def nIters(lhs: String, cchain: CounterChain, N: String, P: String): Unit = {
    cchain.counters.zipWithIndex.foreach { case (counter, i) =>
      emit(src"// Number of iterations of counter #$i")
      emit(src"${lhs}_counter${i}_N = ceil( ceil((${counter.start} - ${counter.end}) / ${counter.step}) / ${lhs}_counter${i}_P)")
    }
    if (cchain.counters.isEmpty) {
      emit(src"$N = 1.0")
      emit(src"$P = 1.0")
    }
    else {
      emit(src"$P = ${cchain.counters.indices.map { i => src"${lhs}_counter${i}_N" }.mkString(" * ")}")
      emit(src"$name = ${cchain.counters.indices.map { i => src"${lhs}_counter${i}_P" }.mkString(" * ")}")
    }
  }

  // def memPars(lhs: String, mem: Sym[_]): Seq[String] = mem match {
  //   case Op(alias: MemDenseAlias[_, _, _]) => alias.ranges.zipWithIndex.map { case (rng, d) =>
  //     val series: Series[Idx] = rng.last
  //     series.par match {case Final(s) => s"$s"; case Expect(s) => s"$s"; case _ => s"$par"}
  //   }
  //   case Op(mem: MemAlloc[_, _]) => (1 to mem.rank.length).map { d =>
  //     "1"
  //   }
  //   case _ => throw new Exception(s"Unknown memory type for symbol $mem")
  // }

  def memSizes(lhs: String, mem: Sym[_]): Unit = {
    val rank = mem match {
      case Op(alias: MemDenseAlias[_, _, _]) =>
        alias.ranges.zipWithIndex.foreach { case (rng, d) =>
          val series: Series[Idx] = rng.last
          emit(src"${lhs}_dim$d = ( (${series.end} - ${series.start} + ${series.step} - 1)/${series.step})")
        }
        alias.rawRank.length
      case Op(alloc: MemAlloc[_, _]) =>
        (1 to alloc.rank.length).foreach{d =>
          emit(src"# Parallelization in dimension #d")
          emit(src"${lhs}_dim$d = ${alloc.dims(d)}")
        }
        alloc.rank.length
      case _ => throw new Exception(s"Unknown memory type for symbol $mem")
    }
    emit(src"${lhs}_dims = [${(1 to rank).map{d => src"${lhs}_dim$d" }.mkString(",")}]")
    emit(src"${lhs}_pa modelrs = [${(1 to rank).map{d => src"${lhs}_P$d" }.mkString(",")}]")
  }

  override protected def emitEntry(block: Block[_]): Unit = {
    emit(src"package model")
    emit(src"import models.Runtime._")
    emit(src"")
    open(src"object AppRuntimeModel_${version} extends App {")
      open(src"def build_model(): ControllerModel = {")
        visitBlock(block)

      close("}")
      
      val gen_dir = if (config.genDir.startsWith("/")) config.genDir + "/" else config.cwd + s"/${config.genDir}"
      val example = if (version == "dse") IR.dseModelArgs.toString else IR.finalModelArgs.toString

      emit("")
      emit("""override def main(args: Array[String]): Unit = {""")
      emit(s"""  begin("$gen_dir/results_$version")""")
      emit("""  if (args.size >= 1 && (args.contains("noninteractive") || args.contains("ni"))) {""")
      emit("""      interactive = false""")
      emit("""      val idx = {0 max args.indexOf("noninteractive")} + {0 max args.indexOf("ni")}""")
      emit("""      cliParams = args.drop(idx+1).takeWhile{_ != "tune"}.map(_.toInt)""")
      emit("""      emit(s"Noninteractive Args: ${cliParams.mkString(" ")}") """)
      emit("""  }""")
      emit("""  else {""")
      emit(s"""    println(s"Suggested args: ${example}")""")
      emit("""  }""")
      emit("""  val allTuneParams: Seq[Map[String, Any]] = if (args.size >= 1 && (args.contains("tune"))) {""")
      emit("""      retune = true""")
      emit("""      val indices: Seq[Int] = args.zipWithIndex.filter(_._1 == "tune").map(_._2)""")
      emit("""      indices.map{idx => args.drop(idx+1).takeWhile{x => x != "noninteractive" && x != "ni" && x != "tune"}.grouped(2).map{x => (x(0) -> {try {x(1).toInt} catch {case _: Throwable => x(1)}} )}.toMap}""")
      emit("""  } else {Seq(Map[String, Any]())}""")
      if (version == "final") emit("  isFinal = true")
      emit("""  val root = build_model()""")
      emit("""  root.initializeAskMap(AskMap.map)""")
      emit("""  root.loadPreviousAskMap(PreviousAskMap.map) // Load previous run's askmap""")
      emit(s"""  emit(s"[$version] Structure for app ${config.name}")""")
      emit("""  allTuneParams.foreach{tuneTo => """)
      emit("""      tuneParams = tuneTo""")
      emit("""      root.printStructure()""")
      emit("""      root.execute()""")
      emit(s"""      emit(s"[$version] Runtime results for app ${config.name}")""")
      emit("""      root.printResults()""")
      emit(s"""      root.storeAskMap("${gen_dir}/model/PreviousAskMap.scala") // Store this run's askmap""")
      emit(s"""      emit(s"[$version] Total Cycles for App ${config.name}: $${root.totalCycles()}")""")
      emit("""  }""")
      emit("""  end()""")
      emit("""}""")
      close("""}""")

    if (version == "dse") {
      withGen(out, "InitAskMap.scala"){
        emit(src"package model")
        open(src"object AskMap {")
          emit(src"val map = scala.collection.mutable.Map[Int,Int]()")
          undefinedSyms.collect{case sym if !isTuneable(sym) =>
            val value = sym match{case Param(c) => s"$c"; case Upper(c) => s"$c"; case _ => "100" } // TODO: Choose some default value. Should warn?
            emit(src"""map += (${sym.hashCode} -> $value)""")
          }
        close("}")
      }
      withGen(out, "PreviousAskMap.scala"){
        emit("package model")
        open("object PreviousAskMap{val map = scala.collection.mutable.Map[Int,Int]()}")
      }
      withGen(out, "AppSensitivity.scala"){
        emit("package model")
        emit("import models.Sensitivity")
        emit("")
        open("object AppSensitivity extends App {")
          open(s"override def main(args: Array[String]): Unit = {")
            val center = TileSizes.all.map{t => t.name.get -> t.intValueOrLowest.toString } ++ ParParams.all.map{p => p.name.get -> p.intValueOrLowest.toString } ++ PipelineParams.all.map{ m => m.toString -> {if (m.schedValue == Pipelined) "true" else "false"} }
            val center_string = center.map{case (p,v) => s""" "$p" -> "$v" """}.mkString("Map(", ",", ")")
            emit(s"""val center: Map[String,String] = $center_string""")
            emit(s"""println(s"Center: $$center") """)
            emit("""println(s"Hashcode mapping:")""")
            (TileSizes.all ++ ParParams.all ++ PipelineParams.all).foreach{x => emit(s"""println(s"${x.name.getOrElse(x.toString)} = ${x.hashCode}")""")}
            emit(s"""Sensitivity.around("$gen_dir/${config.name}_data.csv", center)""")
          close("}")
        close("}")
      }
    }
  }

  protected def bitWidth(tp: Type[_]): Int = tp match {
    case Bits(bT) => bT.nbits
    case _ => -1
  }

  protected def createCtrObject(lhs: Sym[_], start: Sym[_], stop: Sym[_], step: Sym[_], par: Sym[_], forever: Boolean, sfx: String = ""): Unit = {
    val w = try {bitWidth(lhs.tp.typeArgs.head)} catch {case e: Exception => 32}
    val ctx = s"""Ctx("$lhs$sfx", "${lhs.ctx.line}", "${getCtx(lhs).replace("\"","'")}", "${stm(lhs)}")"""
    val strt = start match {
                 case _ if forever => "0"
                 case Final(s) => src"$s"
                 case Upper(s) => undefinedSyms += start; src"""Ask(${start.hashCode}, "ctr start", $ctx)"""
                 case Expect(s) if isTuneable(start) => src"""Tuneable("${start.name.get}", $s, "$start") """
                 case Expect(s) => src"$s"
                 case Param(s) => undefinedSyms += start; src"""Ask(${start.hashCode}, "ctr start", $ctx)"""
                 case Op(RegRead(x)) if x.isArgIn => src"""Ask(${x.hashCode}, "ArgIn $x (ctr start)", $ctx)"""
                 case _ => src"""Ask(${start.hashCode}, "ctr start", $ctx)"""
                }
    val question = 
      if (sfx.contains("_ctr")) s"""length of dim #${sfx.replace("_ctr","")}""" 
      else if (sfx.contains("_fsm")) s"expected # iters for fsm"
      else "ctr stop"
    val stp = stop match {
                 case _ if forever => "Some(5)"
                 case Final(s) => src"$s"
                 case Upper(s) => undefinedSyms += stop; src"""Ask(${stop.hashCode}, "$question", $ctx)"""
                 case Expect(s) if isTuneable(stop) => src"""Tuneable("${stop.name.get}", $s, "${stop}") """
                 case Expect(s) => src"$s"
                 case Param(s) => undefinedSyms += stop; src"""Ask(${stop.hashCode}, "$question", $ctx)"""
                 case Op(RegRead(x)) if x.isArgIn => src"""Ask(${x.hashCode}, "ArgIn $x ($question)", $ctx)"""
                 case _ => src"""Ask(${stop.hashCode}, "$question", $ctx)"""
                }
    val ste = step match {
                 case _ if forever => "Some(0)"
                 case Final(s) => src"$s"
                 case Upper(s) => undefinedSyms += step; src"""Ask(${step.hashCode}, "ctr step", $ctx)"""
                 case Expect(s) if isTuneable(step) => src"""Tuneable("${step.name.get}", $s, "${step}") """
                 case Expect(s) => src"$s"
                 case Param(s) => undefinedSyms += step; src"""Ask(${stop.hashCode}, "ctr step", $ctx)"""
                 case Op(RegRead(x)) if x.isArgIn => src"""Ask(${x.hashCode}, "ArgIn $x (ctr step)", $ctx)"""
                 case _ => src"""Ask(${step.hashCode}, "ctr step", $ctx)"""
                }
    val p = par match {
                 case Final(s) => src"$s"
                 case Upper(s) => undefinedSyms += par; src"""Ask(${par.hashCode}, "ctr par", $ctx)"""
                 case Expect(s) if isTuneable(par) => src"""Tuneable("${par.name.get}", $s, "${par}") """
                 case Expect(s) => src"$s"
                 case Param(s) => undefinedSyms += par; src"""Ask(${par.hashCode}, "ctr par", $ctx)"""
                 case _ => src"""Ask(${par.hashCode}, "ctr par", $ctx)"""
    }
    emit(src"val $lhs$sfx = CtrModel($strt, $stp, $ste, $p)")
  }

  protected def createCChainObject(lhs: Sym[_], ctrs: Seq[Sym[_]]): Unit = {
    val ctx = s"""Ctx("$lhs", "${lhs.ctx.line}", "${getCtx(lhs).replace("\"","'")}", "${ctrs.map{stm}}")"""
    emit(src"""val $lhs = CChainModel(List[CtrModel[_,_,_,_]](${ctrs.map(quote).mkString(",")}), $ctx)""")
  }

  override def gen(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    // case AccelScope(block) if lhs.isInnerControl =>
    //   val body = latencyOfPipe(block).toInt
    //   val ctx = s"""Ctx("$lhs", "${lhs.ctx.line}", "${lhs.ctx.content.getOrElse("???")}", "${stm(lhs)}")"""
    //   emit(src"val $lhs = new ControllerModel(${lhs.level.toString}, ${lhs.rawSchedule.toString}, 1, $body, 1, $ctx)")
    //   emit(src"${lhs}")

    case CounterNew(start,end,step,par) => createCtrObject(lhs, start, end, step, par, false) //createCtr(lhs,start,end,step,par)
    case CounterChainNew(ctrs) => createCChainObject(lhs,ctrs)

    case AccelScope(block) =>
      val lat = if (lhs.isInnerControl) scrubNoise(lhs.bodyLatency.sum) else 0.0
      val ii = if (lhs.II <= 1 | lhs.isOuterControl) 1.0 else scrubNoise(lhs.II)
      val ctx = s"""Ctx("$lhs", "${lhs.ctx.line}", "${getCtx(lhs).replace("\"","'")}", "${stm(lhs)}")"""
      emit(src"val $lhs = new ControllerModel(${lhs.hashCode}, ${lhs.level.toString}, Left(${lhs.rawSchedule.toString}), CChainModel(Seq()), ${lat.toInt}, ${ii.toInt}, $ctx)")
      visitBlock(block)
      lhs.children.filter(_.s.get != lhs).foreach{x => emit(src"$lhs.registerChild(${x.s.get})")}
      emit(src"${lhs}")

    case OpForeach(_,cchain, _,_,_) if lhs.getLoweredTransfer.isDefined =>
      // TODO: Include last level counter?
      val gated = if (lhs.children.count(_.s.get != lhs) == 1) "Gated" else ""
      val ctx = s"""Ctx("$lhs", "${lhs.ctx.line}", "${getCtx(lhs).replace("\"","'")}", "${stm(lhs)}")"""
      val lat = if (lhs.isInnerControl) scrubNoise(lhs.bodyLatency.sum) else 0.0
      val ii = if (lhs.II <= 1 | lhs.isOuterControl) 1.0 else scrubNoise(lhs.II)
      createCtrObject(lhs, Bits[I32].zero,lhs.loweredTransferSize._1,Bits[I32].one,lhs.loweredTransferSize._2, false, s"_ctrlast")
      emit(src"val ${lhs} = new ControllerModel(${lhs.hashCode}, ${lhs.level.toString}, Left(${gated}${lhs.loweredTransfer.toString}), List($cchain, CChainModel(List(${lhs}_ctrlast))), ${lat.toInt}, ${ii.toInt}, $ctx, bitsPerCycle = ${lhs.loweredTransferSize._3}.toDouble)")

    case OpForeach(_,cchain,block,_,_) =>
      val ctx = s"""Ctx("$lhs", "${lhs.ctx.line}", "${getCtx(lhs).replace("\"","'")}", "${stm(lhs)}")"""
      val tuneableSched = if (isTuneable(lhs)) s"""Right(Tuneable("${lhs}", "${lhs.rawSchedule.toString == "Pipelined"}", "${lhs.ctx.line}"))""" else s"Left(${lhs.rawSchedule.toString})"
      val lat = if (lhs.isInnerControl) scrubNoise(lhs.bodyLatency.sum) else 0.0
      val ii = if (lhs.II <= 1 | lhs.isOuterControl) 1.0 else scrubNoise(lhs.II)
      emit(src"val ${lhs} = new ControllerModel(${lhs.hashCode}, ${lhs.level.toString}, ${tuneableSched}, $cchain, ${lat.toInt}, ${ii.toInt}, $ctx)")
      visitBlock(block)
      lhs.children.filter(_.s.get != lhs).foreach{x => emit(src"$lhs.registerChild(${x.s.get})")}

    case UnrolledForeach(_,cchain,_,_,_,_) if lhs.getLoweredTransfer.isDefined =>
      // TODO: Include last level counter?
      val gated = if (lhs.children.filter(_.s.get != lhs).size == 1) "Gated" else ""
      val ctx = s"""Ctx("$lhs", "${lhs.ctx.line}", "${getCtx(lhs).replace("\"","'")}", "${stm(lhs)}")"""
      val lat = if (lhs.isInnerControl) scrubNoise(lhs.bodyLatency.sum) else 0.0
      val ii = if (lhs.II <= 1 | lhs.isOuterControl) 1.0 else scrubNoise(lhs.II)
      createCtrObject(lhs, Bits[I32].zero,lhs.loweredTransferSize._1,Bits[I32].one,lhs.loweredTransferSize._2, false, s"_ctrlast")
      emit(src"val $lhs = new ControllerModel(${lhs.hashCode}, ${lhs.level.toString}, Left($gated${lhs.loweredTransfer.toString}), List($cchain, CChainModel(List(${lhs}_ctrlast))), ${lat.toInt}, ${ii.toInt}, $ctx, bitsPerCycle = ${lhs.loweredTransferSize._3}.toDouble)")

    case UnrolledForeach(_,cchain,func, _, _, _) =>
      val ctx = s"""Ctx("$lhs", "${lhs.ctx.line}", "${getCtx(lhs).replace("\"","'")}", "${stm(lhs)}")"""
      val lat = if (lhs.isInnerControl) scrubNoise(lhs.bodyLatency.sum) else 0.0
      val ii = if (lhs.II <= 1 | lhs.isOuterControl) 1.0 else scrubNoise(lhs.II)
      emit(src"val $lhs = new ControllerModel(${lhs.hashCode}, ${lhs.level.toString}, Left(${lhs.rawSchedule.toString}), $cchain, ${lat.toInt}, ${ii.toInt}, $ctx)")
      visitBlock(func)
      lhs.children.filter(_.s.get != lhs).foreach{x => emit(src"$lhs.registerChild(${x.s.get})")}

    case ParallelPipe(_,block) =>
      val ctx = s"""Ctx("$lhs", "${lhs.ctx.line}", "${getCtx(lhs).replace("\"","'")}", "${stm(lhs)}")"""
      emit(src"val ${lhs} = new ControllerModel(${lhs.hashCode}, ${lhs.level.toString}, Left(${lhs.rawSchedule.toString}), CChainModel(Seq()), 0, 0, $ctx)")
      visitBlock(block)
      lhs.children.filter(_.s.get != lhs).foreach{x => emit(src"$lhs.registerChild(${x.s.get})")}


    case UnitPipe(_, _, _) if lhs.getLoweredTransfer.isDefined =>
      val gated = if (lhs.children.count(_.s.get != lhs) == 1) "Gated" else ""
      val ctx = s"""Ctx("$lhs", "${lhs.ctx.line}", "${getCtx(lhs).replace("\"","'")}", "${stm(lhs)}")"""
      val lat = if (lhs.isInnerControl) scrubNoise(lhs.bodyLatency.sum) else 0.0
      val ii = if (lhs.II <= 1 | lhs.isOuterControl) 1.0 else scrubNoise(lhs.II)
      createCtrObject(lhs, Bits[I32].zero,lhs.loweredTransferSize._1,Bits[I32].one,lhs.loweredTransferSize._2, false, s"_ctrlast")
      emit(src"val $lhs = new ControllerModel(${lhs.hashCode}, ${lhs.level.toString}, Left($gated${lhs.loweredTransfer.toString}), List(CChainModel(Seq()), CChainModel(Seq(${lhs}_ctrlast))), ${lat.toInt}, ${ii.toInt}, $ctx, bitsPerCycle = ${lhs.loweredTransferSize._3}.toDouble)")

    case UnitPipe(_, block, _) =>
      val ctx = s"""Ctx("$lhs", "${lhs.ctx.line}", "${getCtx(lhs).replace("\"","'")}", "${stm(lhs)}")"""
      val lat = if (lhs.isInnerControl) scrubNoise(lhs.bodyLatency.sum) else 0.0
      val ii = if (lhs.II <= 1 | lhs.isOuterControl) 1.0 else scrubNoise(lhs.II)
      emit(src"val $lhs = new ControllerModel(${lhs.hashCode}, ${lhs.level.toString}, Left(${lhs.rawSchedule.toString}), CChainModel(Seq()), ${lat.toInt}, ${ii.toInt}, $ctx)")
      visitBlock(block)
      lhs.children.filter(_.s.get != lhs).foreach{x => emit(src"$lhs.registerChild(${x.s.get})")}

    case OpReduce(_, cchain, _, map, load, reduce, store, _,_,_,_) =>
      val ctx = s"""Ctx("$lhs", "${lhs.ctx.line}", "${getCtx(lhs).replace("\"","'")}", "${stm(lhs)}")"""
      val tuneableSched = if (isTuneable(lhs)) s"""Right(Tuneable("$lhs", "${lhs.rawSchedule.toString == "Pipelined"}", "${lhs.ctx.line}"))""" else s"Left(${lhs.rawSchedule.toString})"
      val lat = if (lhs.isInnerControl) scrubNoise(lhs.bodyLatency.sum) else 0.0
      val ii = if (lhs.II <= 1 | lhs.isOuterControl) 1.0 else scrubNoise(lhs.II)
      emit(src"val $lhs = new ControllerModel(${lhs.hashCode}, ${lhs.level.toString}, $tuneableSched, $cchain, ${lat.toInt}, ${ii.toInt}, $ctx)")
      visitBlock(map)
      visitBlock(load)
      visitBlock(reduce)
      visitBlock(store)
      lhs.children.filter(_.s.get != lhs).foreach{x => emit(src"$lhs.registerChild(${x.s.get})")}

    case UnrolledReduce(_,cchain,func, _, _, _) =>
      val ctx = s"""Ctx("$lhs", "${lhs.ctx.line}", "${getCtx(lhs).replace("\"","'")}", "${stm(lhs)}")"""
      val lat = if (lhs.isInnerControl) scrubNoise(lhs.bodyLatency.sum) else 0.0
      val ii = if (lhs.II <= 1 | lhs.isOuterControl) 1.0 else scrubNoise(lhs.II)
      emit(src"val $lhs = new ControllerModel(${lhs.hashCode}, ${lhs.level.toString}, Left(${lhs.rawSchedule.toString}), $cchain, ${lat.toInt}, ${ii.toInt}, $ctx)")
      visitBlock(func)
      lhs.children.filter(_.s.get != lhs).foreach{x => emit(src"$lhs.registerChild(${x.s.get})")}

    case tx:DenseTransfer[_,_,_] =>
      val ctx = s"""Ctx("$lhs", "${lhs.ctx.line}", "${getCtx(lhs).replace("\"","'")}", "${stm(lhs)}")"""

      val tp = if (tx.isStore) "DenseStore" else "DenseLoad"
      val pars = tx.pars.map(_.asInstanceOf[Sym[_]])
                                        // .map{p => p match {
                                        //          case Final(s) => src"${s.toInt}"
                                        //          case Upper(s) => undefinedSyms += p; src"""Ask(${p.hashCode}, "load par", $ctx)"""
                                        //          case Expect(s) if (isTuneable(p)) => src"""Tuneable(${p.hashCode}, $s, "${p}") """
                                        //          case Expect(s) => src"${s.toInt}"
                                        //          case Param(s) => undefinedSyms += p; src"""Ask(${p.hashCode}, "load par", $ctx)"""
                                        //          case _ => src"""Ask(${p.hashCode}, "load par", $ctx)"""
                                        // }}
      val steps = tx.ctrSteps.map(_.asInstanceOf[Sym[_]])
      val lens = tx.lens.map(_.asInstanceOf[Sym[_]])

      // Generate ctrs
      List.tabulate(pars.size){i => createCtrObject(lhs, Bits[I32].zero,lens(i),steps(i),pars(i), false, s"_ctr$i")}
      emit(src"""val ${lhs}_cchain = List(CChainModel(List[CtrModel[_,_,_,_]](${pars.dropRight(1).zipWithIndex.map{case (_,i) => s"${lhs}_ctr$i"}.mkString(",")}), $ctx), CChainModel(List(${lhs}_ctr${pars.size-1}), $ctx))""")

      val lat = 0.0
      val ii = 0.0
      emit(src"val $lhs = new ControllerModel(${lhs.hashCode}, OuterControl, $tp, ${lhs}_cchain, ${lat.toInt}, ${ii.toInt}, $ctx)")


    case StateMachine(_, _, notDone, action, nextState) =>
      val ctx = s"""Ctx("$lhs", "${lhs.ctx.line}", "${getCtx(lhs).replace("\"","'")}", "${stm(lhs)}")"""
      val lat = if (lhs.isInnerControl) scrubNoise(lhs.bodyLatency.sum) else 0.0
      val ii = if (lhs.II <= 1 | lhs.isOuterControl) 1.0 else scrubNoise(lhs.II)
      createCtrObject(lhs, Bits[I32].zero,lhs,Bits[I32].one,Bits[I32].one, false, s"_fsm")
      emit(src"""val ${lhs}_cchain = CChainModel(List[CtrModel[_,_,_,_]](${lhs}_fsm), $ctx)""")
      emit(src"val $lhs = new ControllerModel(${lhs.hashCode}, ${lhs.level.toString}, Left(${lhs.rawSchedule.toString}), ${lhs}_cchain, ${lat.toInt} + 2, ${ii.toInt} + 2, $ctx)") // TODO: Add 2 because it seems to be invisible latency?
      visitBlock(notDone)
      visitBlock(action)
      visitBlock(nextState)
      lhs.children.filter(_.s.get != lhs).foreach{x => emit(src"$lhs.registerChild(${x.s.get})")}

    case OpMemReduce(_, cchainMap, cchainRed, _, map, loadRes, loadAcc, _, storeAcc, _, _, _, _, _) =>
      val ctx = s"""Ctx("$lhs", "${lhs.ctx.line}", "${getCtx(lhs).replace("\"","'")}", "${stm(lhs)}")"""
      val tuneableSched = if (isTuneable(lhs)) s"""Right(Tuneable("${lhs}", "${lhs.rawSchedule.toString == "Pipelined"}", "${lhs.ctx.line}"))""" else s"Left(${lhs.rawSchedule.toString})"
      val lat = if (lhs.isInnerControl) scrubNoise(lhs.bodyLatency.sum) else 0.0
      val ii = if (lhs.II <= 1 | lhs.isOuterControl) 1.0 else scrubNoise(lhs.II)
      emit(src"val ${lhs} = new ControllerModel(${lhs.hashCode}, ${lhs.level.toString}, $tuneableSched, List($cchainMap, $cchainRed), ${lat.toInt}, ${ii.toInt}, $ctx)")
      visitBlock(map)
      visitBlock(loadRes)
      visitBlock(loadAcc)
      visitBlock(storeAcc)
      lhs.children.filter(_.s.get != lhs).foreach{x => emit(src"$lhs.registerChild(${x.s.get})")}


    case Switch(_, body) =>
      val ctx = s"""Ctx("$lhs", "${lhs.ctx.line}", "${getCtx(lhs).replace("\"","'")}", "${stm(lhs)}")"""
      val lat = if (lhs.isInnerControl) scrubNoise(lhs.bodyLatency.sum) else 0.0
      val ii = if (lhs.II <= 1 | lhs.isOuterControl) 1.0 else scrubNoise(lhs.II)
      visitBlock(body)
      emit(src"val $lhs = new ControllerModel(${lhs.hashCode}, OuterControl, Left(${lhs.rawSchedule.toString}), CChainModel(List()), ${lat.toInt} + 2, ${ii.toInt} + 2, $ctx)") // TODO: Add 2 because it seems to be invisible latency?
      lhs.children.filter(_.s.get != lhs).foreach{x => emit(src"$lhs.registerChild(${x.s.get})")}

    case SwitchCase(body) =>
      val ctx = s"""Ctx("$lhs", "${lhs.ctx.line}", "${getCtx(lhs).replace("\"","'")}", "${stm(lhs)}")"""
      val lat = if (lhs.isInnerControl) scrubNoise(lhs.bodyLatency.sum) else 0.0
      val ii = if (lhs.II <= 1 | lhs.isOuterControl) 1.0 else scrubNoise(lhs.II)
      visitBlock(body)
      emit(src"val $lhs = new ControllerModel(${lhs.hashCode}, ${lhs.level.toString}, Left(${lhs.rawSchedule.toString}), CChainModel(List()), ${lat.toInt} + 2, ${ii.toInt} + 2, $ctx)") // TODO: Add 2 because it seems to be invisible latency?
      lhs.children.filter(_.s.get != lhs).foreach{x => emit(src"$lhs.registerChild(${x.s.get})")}

    case _ => lhs.blocks.foreach{block => visitBlock(block) }
  }

  override def copyDependencies(out: String): Unit = {
    dependencies ::= DirDep("synth", "project", "../", Some("project/"))
    dependencies ::= DirDep("synth", "scripts", "../", Some("scripts/"))
    dependencies ::= FileDep("synth", "build.sbt", "../", Some("build.sbt"))
    super.copyDependencies(out)
  }
}