// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import chisel3.experimental.DataMirror
import chisel3.util._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink.Util.wrappingAdd
import freechips.rocketchip.util._


// some utilities function
object Util {
  def wrappingAdd(u: UInt, n: UInt, max_plus_one: UInt, en: Bool = true.B): UInt = {
    val max = max_plus_one - 1.U
    assert(n <= max || max === 0.U, "cannot wrapAdd when n is larger than max, unless max is 0")

    /*
  Mux(!en, u,
    Mux (max === 0.U, 0.U,
      Mux(u >= max - n + 1.U && n =/= 0.U, n - (max - u) - 1.U, u + n)))
  */

    MuxCase(u + n, Seq(
      (!en) -> u,
      (max === 0.U) -> 0.U,
      (u >= max - n + 1.U && n =/= 0.U) -> (n - (max - u) - 1.U)
    ))
  }
}
// Trades off slave port proximity against routing resource cost
object ForceFanout
{
  def apply[T](
                a: TriStateValue = TriStateValue.unset,
                b: TriStateValue = TriStateValue.unset,
                c: TriStateValue = TriStateValue.unset,
                d: TriStateValue = TriStateValue.unset,
                e: TriStateValue = TriStateValue.unset)(body: Parameters => T)(implicit p: Parameters) =
  {
    body(p.alterPartial {
      case ForceFanoutKey => p(ForceFanoutKey) match {
        case ForceFanoutParams(pa, pb, pc, pd, pe) =>
          ForceFanoutParams(a.update(pa), b.update(pb), c.update(pc), d.update(pd), e.update(pe))
      }
    })
  }
}

private case class ForceFanoutParams(a: Boolean, b: Boolean, c: Boolean, d: Boolean, e: Boolean)
private case object ForceFanoutKey extends Field(ForceFanoutParams(false, false, false, false, false))

class TLXbar(policy: TLArbiter.Policy = TLArbiter.roundRobin)(implicit p: Parameters) extends LazyModule
{
  val node = new TLNexusNode(
    clientFn  = { seq =>
      seq(0).v1copy(
        echoFields    = BundleField.union(seq.flatMap(_.echoFields)),
        requestFields = BundleField.union(seq.flatMap(_.requestFields)),
        responseKeys  = seq.flatMap(_.responseKeys).distinct,
        minLatency = seq.map(_.minLatency).min,
        clients = (TLXbar.mapInputIds(seq) zip seq) flatMap { case (range, port) =>
          port.clients map { client => client.v1copy(
            sourceId = client.sourceId.shift(range.start)
          )}
        }
      )
    },
    managerFn = { seq =>
      val fifoIdFactory = TLXbar.relabeler()
      seq(0).v1copy(
        responseFields = BundleField.union(seq.flatMap(_.responseFields)),
        //        responseFields = BundleField.union(seq.flatMap(_.responseFields) :+ GemminiPriField()),
        requestKeys = seq.flatMap(_.requestKeys).distinct,
        minLatency = seq.map(_.minLatency).min,
        endSinkId = TLXbar.mapOutputIds(seq).map(_.end).max,
        managers = seq.flatMap { port =>
          require (port.beatBytes == seq(0).beatBytes,
            s"Xbar ($name with parent $parent) data widths don't match: ${port.managers.map(_.name)} has ${port.beatBytes}B vs ${seq(0).managers.map(_.name)} has ${seq(0).beatBytes}B")
          val fifoIdMapper = fifoIdFactory()
          port.managers map { manager => manager.v1copy(
            fifoId = manager.fifoId.map(fifoIdMapper(_))
          )}
        }
      )
    }
  ){
    override def circuitIdentity = outputs.size == 1 && inputs.size == 1
  }

  lazy val module = new LazyModuleImp(this) {
    if ((node.in.size * node.out.size) > (8*32)) {
      println (s"!!! WARNING !!!")
      println (s" Your TLXbar ($name with parent $parent) is very large, with ${node.in.size} Masters and ${node.out.size} Slaves.")
      println (s"!!! WARNING !!!")
    }

    TLXbar.circuit(policy, node.in, node.out)
  }
}

class TLXbar_ACancel(policy: TLArbiter.Policy = TLArbiter.roundRobin)(implicit p: Parameters) extends LazyModule
{
  val node = new TLNexusNode_ACancel(
    clientFn  = { seq =>
      seq(0).v1copy(
        echoFields    = BundleField.union(seq.flatMap(_.echoFields)),
        requestFields = BundleField.union(seq.flatMap(_.requestFields)),
        responseKeys  = seq.flatMap(_.responseKeys).distinct,
        minLatency = seq.map(_.minLatency).min,
        clients = (TLXbar.mapInputIds(seq) zip seq) flatMap { case (range, port) =>
          port.clients map { client => client.v1copy(
            sourceId = client.sourceId.shift(range.start)
          )}
        }
      )
    },
    managerFn = { seq =>
      val fifoIdFactory = TLXbar.relabeler()
      seq(0).v1copy(
        responseFields = BundleField.union(seq.flatMap(_.responseFields)),
        requestKeys = seq.flatMap(_.requestKeys).distinct,
        minLatency = seq.map(_.minLatency).min,
        endSinkId = TLXbar.mapOutputIds(seq).map(_.end).max,
        managers = seq.flatMap { port =>
          require (port.beatBytes == seq(0).beatBytes,
            s"Xbar ($name with parent $parent) data widths don't match: ${port.managers.map(_.name)} has ${port.beatBytes}B vs ${seq(0).managers.map(_.name)} has ${seq(0).beatBytes}B")
          val fifoIdMapper = fifoIdFactory()
          port.managers map { manager => manager.v1copy(
            fifoId = manager.fifoId.map(fifoIdMapper(_))
          )}
        }
      )
    })
  {
    override def circuitIdentity = outputs == 1 && inputs == 1
  }

  lazy val module = new LazyModuleImp(this) {
    if ((node.in.size * node.out.size) > (8*32)) {
      println (s"!!! WARNING !!!")
      println (s" Your TLXbar ($name with parent $parent) is very large, with ${node.in.size} Masters and ${node.out.size} Slaves.")
      println (s"!!! WARNING !!!")
    }

    TLXbar_ACancel.circuit(policy, node.in, node.out)
  }
}

object TLXbar
{
  def circuit(policy: TLArbiter.Policy, seqIn: Seq[(TLBundle, TLEdge)], seqOut: Seq[(TLBundle, TLEdge)]): Unit = {
    val seqOut_ACancel = seqOut.map(sOut => (Wire(new TLBundle_ACancel(sOut._1.params)), sOut._2))
    val seqIn_ACancel = seqIn.map(sIn => (TLBundle_ACancel(sIn._1), sIn._2))
    TLXbar_ACancel.circuit(policy, seqIn_ACancel, seqOut_ACancel)
    (seqOut.map(_._1) zip seqOut_ACancel.map(_._1)) foreach { case (sOut, sOut_ACancel) =>
      sOut <> sOut_ACancel.asDecoupled()
    }
  }

  def apply(policy: TLArbiter.Policy = TLArbiter.roundRobin)(implicit p: Parameters): TLNode =
  {
    val xbar = LazyModule(new TLXbar(policy))
    xbar.node
  }

  def mapInputIds (ports: Seq[TLMasterPortParameters]) = assignRanges(ports.map(_.endSourceId))
  def mapOutputIds(ports: Seq[TLSlavePortParameters ]) = assignRanges(ports.map(_.endSinkId))

  def assignRanges(sizes: Seq[Int]) = {
    val pow2Sizes = sizes.map { z => if (z == 0) 0 else 1 << log2Ceil(z) }
    val tuples = pow2Sizes.zipWithIndex.sortBy(_._1) // record old index, then sort by increasing size
    val starts = tuples.scanRight(0)(_._1 + _).tail // suffix-sum of the sizes = the start positions
    val ranges = (tuples zip starts) map { case ((sz, i), st) =>
      (if (sz == 0) IdRange(0,0) else IdRange(st, st+sz), i)
    }
    ranges.sortBy(_._2).map(_._1) // Restore original order
  }

  def relabeler() = {
    var idFactory = 0
    () => {
      val fifoMap = scala.collection.mutable.HashMap.empty[Int, Int]
      (x: Int) => {
        if (fifoMap.contains(x)) fifoMap(x) else {
          val out = idFactory
          idFactory = idFactory + 1
          fifoMap += (x -> out)
          out
        }
      }
    }
  }

  // Replicate an input port to each output port
  def fanout[T <: TLChannel](input: DecoupledIO[T], select: Seq[Bool], force: Seq[Boolean] = Nil): Seq[DecoupledIO[T]] = {
    val filtered = Wire(Vec(select.size, chiselTypeOf(input)))
    for (i <- 0 until select.size) {
      filtered(i).bits := (if (force.lift(i).getOrElse(false)) IdentityModule(input.bits) else input.bits)
      filtered(i).valid := input.valid && (select(i) || (select.size == 1).B)
    }
    input.ready := Mux1H(select, filtered.map(_.ready))
    filtered
  }
}

// added for priority encoding
case object GemminiPri extends ControlKey[UInt]("pri_port")
case class GemminiPriField() extends BundleField(GemminiPri) {
  def data = Output(UInt(2.W))
  def default(x: UInt): Unit = {
    x := 0.U
  }
}

object TLXbar_ACancel
{
  def circuit(policy: TLArbiter.Policy, seqIn: Seq[(TLBundle_ACancel, TLEdge)], seqOut: Seq[(TLBundle_ACancel, TLEdge)]): Unit = {
    val (io_in, edgesIn) = seqIn.unzip
    val (io_out, edgesOut) = seqOut.unzip

    // Not every master need connect to every slave on every channel; determine which connections are necessary
    val reachableIO = edgesIn.map { cp => edgesOut.map { mp =>
      cp.client.clients.exists { c => mp.manager.managers.exists { m =>
        c.visibility.exists { ca => m.address.exists { ma =>
          ca.overlaps(ma)}}}}
    }.toVector}.toVector
    val probeIO = (edgesIn zip reachableIO).map { case (cp, reachableO) =>
      (edgesOut zip reachableO).map { case (mp, reachable) =>
        reachable && cp.client.anySupportProbe && mp.manager.managers.exists(_.regionType >= RegionType.TRACKED)
      }.toVector}.toVector
    val releaseIO = (edgesIn zip reachableIO).map { case (cp, reachableO) =>
      (edgesOut zip reachableO).map { case (mp, reachable) =>
        reachable && cp.client.anySupportProbe && mp.manager.anySupportAcquireB
      }.toVector}.toVector

    val connectAIO = reachableIO
    val connectBIO = probeIO
    val connectCIO = releaseIO
    val connectDIO = reachableIO
    val connectEIO = releaseIO

    def transpose[T](x: Seq[Seq[T]]) = if (x.isEmpty) Nil else Vector.tabulate(x(0).size) { i => Vector.tabulate(x.size) { j => x(j)(i) } }
    val connectAOI = transpose(connectAIO)
    val connectBOI = transpose(connectBIO)
    val connectCOI = transpose(connectCIO)
    val connectDOI = transpose(connectDIO)
    val connectEOI = transpose(connectEIO)

    // Grab the port ID mapping
    val inputIdRanges = TLXbar.mapInputIds(edgesIn.map(_.client))
    val outputIdRanges = TLXbar.mapOutputIds(edgesOut.map(_.manager))

    // We need an intermediate size of bundle with the widest possible identifiers
    val wide_bundle = TLBundleParameters.union(io_in.map(_.params) ++ io_out.map(_.params))

    // Handle size = 1 gracefully (Chisel3 empty range is broken)
    def trim(id: UInt, size: Int): UInt = if (size <= 1) 0.U else id(log2Ceil(size)-1, 0)

    // Transform input bundle sources (sinks use global namespace on both sides)
    val in = Wire(Vec(io_in.size, TLBundle_ACancel(wide_bundle)))

    ///////////////////////////////////////// added part////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //    val in_a_que = Seq.fill(io_in.size){Module(new Queue(new TLBundleA(in(0).a.bits.params), 5))} // need change
    val in_a_que = (0 until in.size).map{i => Module(new Queue(new TLBundleA(in(i).a.bits.params), 5))}

    val in_q_que_ios = in_a_que.map(_.io)
    println(in.size)
    println(connectAIO.foreach(println(_)))
    val gemminipri_reorder = WireInit(false.B) // activate reordering?
    val prior_vec_uint = Wire(Vec(in.size, UInt(2.W)))

    val reorder_count = RegInit(0.U(5.W)) // count number of reordering
    val reorder_count_max = WireInit(8.U) // 8:1 ratio (ToDo: configurable)
    val reordered = WireInit(false.B) // whether it is reordered or not (use to count up reorder_count)

    if(in.size != 0){
      val prior_vec = VecInit(Seq.fill(in.size)(false.B))
      val prior_de_vec = VecInit(Seq.fill(in.size)(false.B))

      for(i <- 0 until in.size){
        val prior_vec_reg = RegInit(VecInit(Seq.fill(in.size){0.U(2.W)}))
        in_a_que(i).io.deq.bits.user.lift(GemminiPri).foreach{x => dontTouch(x)}
        prior_vec_uint(i) := 0.U
        //prior_vec(i).foreach{x => dontTouch(x)}
        //getOrElse
        if(!in_a_que(i).io.deq.bits.user.lift(GemminiPri).isEmpty) {
          //prior_vec_uint := (0 until in.size).map{i => in_a_que(i).io.deq.bits.user.lift(GemminiPri).get}
          when(in_a_que(i).io.deq.valid){
            prior_vec_uint(i) := in_a_que(i).io.deq.bits.user.lift(GemminiPri).get
            prior_vec_reg(i) := in_a_que(i).io.deq.bits.user.lift(GemminiPri).get //register the value
          }
          prior_vec(i) := Mux(in_a_que(i).io.deq.valid, prior_vec_uint(i) === 3.U, prior_vec_reg(i) === 3.U)
          prior_de_vec(i) := Mux(in_a_que(i).io.deq.valid, prior_vec_uint(i) =/= 2.U, prior_vec_reg(i) =/= 2.U)

          //          prior_vec(i) := Mux(prior_vec_uint(i) === 3.U, true.B, false.B) // get pri field for the last element in the queue
          //prior_de_vec(i) := Mux(prior_vec_uint(i) === 2.U, false.B, true.B)
        }
      }
      gemminipri_reorder := prior_vec.reduce(_||_) && !prior_de_vec.reduce(_ && _)
      dontTouch(gemminipri_reorder)
      dontTouch(prior_vec_uint)
      dontTouch(prior_vec)
    }

    for (i <- 0 until in.size) {
      val r = inputIdRanges(i)

      if(connectAIO(i).exists(x=>x)){
        in_q_que_ios(i).enq.valid := true.B
        in_q_que_ios(i).deq.ready := true.B

        in_q_que_ios(i).enq :<> io_in(i).a.asDecoupled()
        in_q_que_ios(i).enq.bits.source := io_in(i).a.bits.source | r.start.U
        //in(i).a :<> ReadyValidCancel(in_q_que_ios(i).deq) // it does not work at all here

        // this also does not work (stall right after first reordering)
        in(i).a.earlyValid := in_q_que_ios(i).deq.valid
        in(i).a.lateCancel := false.B
        in(i).a.bits := in_q_que_ios(i).deq.bits
        in_q_que_ios(i).deq.ready := in(i).a.ready


        if(!io_in(i).a.bits.user.lift(GemminiPri).isEmpty){
          in_q_que_ios(i).deq.ready := Mux(gemminipri_reorder && (prior_vec_uint(i) === 2.U) && (reorder_count =/= reorder_count_max - 1.U), false.B, in(i).a.ready)
          in(i).a.earlyValid := Mux(gemminipri_reorder && (prior_vec_uint(i) === 2.U) && (reorder_count =/= reorder_count_max - 1.U), false.B, in_q_que_ios(i).deq.valid)
          in(i).a.lateCancel := Mux(gemminipri_reorder && (prior_vec_uint(i) === 2.U) && (reorder_count =/= reorder_count_max - 1.U), true.B, false.B) // would this work?


          when(gemminipri_reorder && (prior_vec_uint(i) === 3.U) && in_q_que_ios(i).deq.fire()){
            reordered := true.B
          }

          println("gemminipri exist")
          println(io_in(i).a.bits.params)
          //in_q_que_ios(i).enq.bits.user.lift(GemminiPri).foreach{x => x := io_in(i).a.bits.user.lift(GemminiPri).get}  // GemminiPri
        }
        //in(i).a :<> ReadyValidCancel(in_q_que_ios(i).deq) // it works, but not as expected
        dontTouch(in(i).a.ready)


      } else {
        scala.Predef.assert(false)
        throw new RuntimeException()
        /*
        in_q_que_ios(i).enq.bits.earlyValid := false.B
        in_q_que_ios(i).enq.bits.lateCancel := DontCare
        in_q_que_ios(i).enq.bits.bits := DontCare
        // here? or depends on the output of the queue
        io_in(i).a.ready      := true.B
        io_in(i).a.lateCancel := DontCare
        io_in(i).a.bits       := DontCare
         */
      }
      //in(i).a.bits.source := in_a_que(i).io.deq.bits.tl_a.bits.source // what is r start?
      reorder_count := wrappingAdd(reorder_count, 1.U, reorder_count_max, reordered)
      /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

      if (connectBIO(i).exists(x=>x)) {
        io_in(i).b :<> in(i).b
        io_in(i).b.bits.source := trim(in(i).b.bits.source, r.size)
      } else {
        in(i).b.ready := true.B
        in(i).b.bits  := DontCare
        io_in(i).b.valid := false.B
        io_in(i).b.bits  := DontCare
      }

      if (connectCIO(i).exists(x=>x)) {
        in(i).c :<> io_in(i).c
        in(i).c.bits.source := io_in(i).c.bits.source | r.start.U
      } else {
        in(i).c.valid := false.B
        in(i).c.bits  := DontCare
        io_in(i).c.ready := true.B
        io_in(i).c.bits  := DontCare
      }

      if (connectDIO(i).exists(x=>x)) {
        io_in(i).d :<> in(i).d
        io_in(i).d.bits.source := trim(in(i).d.bits.source, r.size)
      } else {
        in(i).d.ready := true.B
        in(i).d.bits  := DontCare
        io_in(i).d.valid := false.B
        io_in(i).d.bits  := DontCare
      }

      if (connectEIO(i).exists(x=>x)) {
        in(i).e :<> io_in(i).e
      } else {
        in(i).e.valid := false.B
        in(i).e.bits  := DontCare
        io_in(i).e.ready := true.B
        io_in(i).e.bits  := DontCare
      }
    }

    // Transform output bundle sinks (sources use global namespace on both sides)
    val out = Wire(Vec(io_out.size, TLBundle_ACancel(wide_bundle)))
    for (o <- 0 until out.size) {
      val r = outputIdRanges(o)

      if (connectAOI(o).exists(x=>x)) {
        io_out(o).a :<> out(o).a
      } else {
        out(o).a.ready      := true.B
        out(o).a.lateCancel := DontCare
        out(o).a.bits       := DontCare
        io_out(o).a.earlyValid := false.B
        io_out(o).a.lateCancel := DontCare
        io_out(o).a.bits       := DontCare
      }

      if (connectBOI(o).exists(x=>x)) {
        out(o).b :<> io_out(o).b
      } else {
        out(o).b.valid := false.B
        out(o).b.bits  := DontCare
        io_out(o).b.ready := true.B
        io_out(o).b.bits  := DontCare
      }

      if (connectCOI(o).exists(x=>x)) {
        io_out(o).c :<> out(o).c
      } else {
        out(o).c.ready := true.B
        out(o).c.bits  := DontCare
        io_out(o).c.valid := false.B
        io_out(o).c.bits  := DontCare
      }

      if (connectDOI(o).exists(x=>x)) {
        out(o).d :<> io_out(o).d
        out(o).d.bits.sink := io_out(o).d.bits.sink | r.start.U
      } else {
        out(o).d.valid := false.B
        out(o).d.bits  := DontCare
        io_out(o).d.ready := true.B
        io_out(o).d.bits  := DontCare
      }

      if (connectEOI(o).exists(x=>x)) {
        io_out(o).e :<> out(o).e
        io_out(o).e.bits.sink := trim(out(o).e.bits.sink, r.size)
      } else {
        out(o).e.ready := true.B
        out(o).e.bits  := DontCare
        io_out(o).e.valid := false.B
        io_out(o).e.bits  := DontCare
      }
    }

    // Filter a list to only those elements selected
    def filter[T](data: Seq[T], mask: Seq[Boolean]) = (data zip mask).filter(_._2).map(_._1)

    // Based on input=>output connectivity, create per-input minimal address decode circuits
    val requiredAC = (connectAIO ++ connectCIO).distinct
    val outputPortFns: Map[Vector[Boolean], Seq[UInt => Bool]] = requiredAC.map { connectO =>
      val port_addrs = edgesOut.map(_.manager.managers.flatMap(_.address))
      val routingMask = AddressDecoder(filter(port_addrs, connectO))
      val route_addrs = port_addrs.map(seq => AddressSet.unify(seq.map(_.widen(~routingMask)).distinct))

      // Print the address mapping
      if (false) {
        println("Xbar mapping:")
        route_addrs.foreach { p =>
          print(" ")
          p.foreach { a => print(s" ${a}") }
          println("")
        }
        println("--")
      }

      (connectO, route_addrs.map(seq => (addr: UInt) => seq.map(_.contains(addr)).reduce(_ || _)))
    }.toMap

    // Print the ID mapping
    if (false) {
      println(s"XBar mapping:")
      (edgesIn zip inputIdRanges).zipWithIndex.foreach { case ((edge, id), i) =>
        println(s"\t$i assigned ${id} for ${edge.client.clients.map(_.name).mkString(", ")}")
      }
      println("")
    }

    val addressA = (in zip edgesIn) map { case (i, e) => e.address(i.a.bits) }
    val addressC = (in zip edgesIn) map { case (i, e) => e.address(i.c.bits) }

    def unique(x: Vector[Boolean]): Bool = (x.filter(x=>x).size <= 1).B
    val requestAIO = (connectAIO zip addressA) map { case (c, i) => outputPortFns(c).map { o => unique(c) || o(i) } }
    val requestCIO = (connectCIO zip addressC) map { case (c, i) => outputPortFns(c).map { o => unique(c) || o(i) } }
    val requestBOI = out.map { o => inputIdRanges.map  { i => i.contains(o.b.bits.source) } }
    val requestDOI = out.map { o => inputIdRanges.map  { i => i.contains(o.d.bits.source) } }
    val requestEIO = in.map  { i => outputIdRanges.map { o => o.contains(i.e.bits.sink) } }

    val beatsAI = (in  zip edgesIn)  map { case (i, e) => e.numBeats1(i.a.bits) }
    val beatsBO = (out zip edgesOut) map { case (o, e) => e.numBeats1(o.b.bits) }
    val beatsCI = (in  zip edgesIn)  map { case (i, e) => e.numBeats1(i.c.bits) }
    val beatsDO = (out zip edgesOut) map { case (o, e) => e.numBeats1(o.d.bits) }
    val beatsEI = (in  zip edgesIn)  map { case (i, e) => e.numBeats1(i.e.bits) }

    // Fanout the input sources to the output sinks
    val portsAOI = transpose((in  zip requestAIO) map { case (i, r) => TLXbar_ACancel.fanout(i.a, r, edgesOut.map(_.params(ForceFanoutKey).a)) })
    val portsBIO = transpose((out zip requestBOI) map { case (o, r) => TLXbar        .fanout(o.b, r, edgesIn .map(_.params(ForceFanoutKey).b)) })
    val portsCOI = transpose((in  zip requestCIO) map { case (i, r) => TLXbar        .fanout(i.c, r, edgesOut.map(_.params(ForceFanoutKey).c)) })
    val portsDIO = transpose((out zip requestDOI) map { case (o, r) => TLXbar        .fanout(o.d, r, edgesIn .map(_.params(ForceFanoutKey).d)) })
    val portsEOI = transpose((in  zip requestEIO) map { case (i, r) => TLXbar        .fanout(i.e, r, edgesOut.map(_.params(ForceFanoutKey).e)) })

    // Arbitrate amongst the sources
    for (o <- 0 until out.size) {
      TLArbiter.applyCancel(policy)(out(o).a, filter(beatsAI zip portsAOI(o), connectAOI(o)):_*)
      TLArbiter            (policy)(out(o).c, filter(beatsCI zip portsCOI(o), connectCOI(o)):_*)
      TLArbiter            (policy)(out(o).e, filter(beatsEI zip portsEOI(o), connectEOI(o)):_*)
      filter(portsAOI(o), connectAOI(o).map(!_)) foreach { r => r.ready := false.B }
      filter(portsCOI(o), connectCOI(o).map(!_)) foreach { r => r.ready := false.B }
      filter(portsEOI(o), connectEOI(o).map(!_)) foreach { r => r.ready := false.B }
    }

    for (i <- 0 until in.size) {
      TLArbiter(policy)(in(i).b, filter(beatsBO zip portsBIO(i), connectBIO(i)):_*)
      TLArbiter(policy)(in(i).d, filter(beatsDO zip portsDIO(i), connectDIO(i)):_*)
      filter(portsBIO(i), connectBIO(i).map(!_)) foreach { r => r.ready := false.B }
      filter(portsDIO(i), connectDIO(i).map(!_)) foreach { r => r.ready := false.B }
    }
  }

  def apply(policy: TLArbiter.Policy = TLArbiter.roundRobin)(implicit p: Parameters): TLNode_ACancel =
  {
    val xbar = LazyModule(new TLXbar_ACancel(policy))
    xbar.node
  }

  // Replicate an input port to each output port
  def fanout[T <: TLChannel](input: ReadyValidCancel[T], select: Seq[Bool], force: Seq[Boolean] = Nil): Seq[ReadyValidCancel[T]] = {
    val filtered = Wire(Vec(select.size, chiselTypeOf(input)))
    for (i <- 0 until select.size) {
      filtered(i).bits := (if (force.lift(i).getOrElse(false)) IdentityModule(input.bits) else input.bits)
      filtered(i).lateCancel := input.lateCancel
      filtered(i).earlyValid := input.earlyValid && (select(i) || (select.size == 1).B)
    }
    input.ready := Mux1H(select, filtered.map(_.ready))
    filtered
  }
}

/** Synthesizeable unit tests */
import freechips.rocketchip.unittest._

class TLRAMXbar(nManagers: Int, txns: Int)(implicit p: Parameters) extends LazyModule {
  val fuzz = LazyModule(new TLFuzzer(txns))
  val model = LazyModule(new TLRAMModel("Xbar"))
  val xbar = LazyModule(new TLXbar)

  xbar.node := TLDelayer(0.1) := model.node := fuzz.node
  (0 until nManagers) foreach { n =>
    val ram  = LazyModule(new TLRAM(AddressSet(0x0+0x400*n, 0x3ff)))
    ram.node := TLFragmenter(4, 256) := TLDelayer(0.1) := xbar.node
  }

  lazy val module = new LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzz.module.io.finished
  }
}

class TLRAMXbarTest(nManagers: Int, txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new TLRAMXbar(nManagers,txns)).module)
  io.finished := dut.io.finished
}

class TLMulticlientXbar(nManagers: Int, nClients: Int, txns: Int)(implicit p: Parameters) extends LazyModule {
  val xbar = LazyModule(new TLXbar)

  val fuzzers = (0 until nClients) map { n =>
    val fuzz = LazyModule(new TLFuzzer(txns))
    xbar.node := TLDelayer(0.1) := fuzz.node
    fuzz
  }

  (0 until nManagers) foreach { n =>
    val ram = LazyModule(new TLRAM(AddressSet(0x0+0x400*n, 0x3ff)))
    ram.node := TLFragmenter(4, 256) := TLDelayer(0.1) := xbar.node
  }

  lazy val module = new LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzzers.last.module.io.finished
  }
}

class TLMulticlientXbarTest(nManagers: Int, nClients: Int, txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new TLMulticlientXbar(nManagers, nClients, txns)).module)
  io.finished := dut.io.finished
}
