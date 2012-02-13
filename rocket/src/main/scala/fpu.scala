package Top

import Chisel._
import Node._
import Constants._
import Instructions._

object rocketFPConstants
{
  val FCMD_ADD =        Bits("b000000")
  val FCMD_SUB =        Bits("b000001")
  val FCMD_MUL =        Bits("b000010")
  val FCMD_DIV =        Bits("b000011")
  val FCMD_SQRT =       Bits("b000100")
  val FCMD_SGNINJ =     Bits("b000101")
  val FCMD_SGNINJN =    Bits("b000110")
  val FCMD_SGNMUL =     Bits("b000111")
  val FCMD_CVT_L_FMT =  Bits("b001000")
  val FCMD_CVT_LU_FMT = Bits("b001001")
  val FCMD_CVT_W_FMT =  Bits("b001010")
  val FCMD_CVT_WU_FMT = Bits("b001011")
  val FCMD_CVT_FMT_L =  Bits("b001100")
  val FCMD_CVT_FMT_LU = Bits("b001101")
  val FCMD_CVT_FMT_W =  Bits("b001110")
  val FCMD_CVT_FMT_WU = Bits("b001111")
  val FCMD_CVT_FMT_S =  Bits("b010000")
  val FCMD_CVT_FMT_D =  Bits("b010001")
  val FCMD_EQ =         Bits("b010101")
  val FCMD_LT =         Bits("b010110")
  val FCMD_LE =         Bits("b010111")
  val FCMD_MIN =        Bits("b011000")
  val FCMD_MAX =        Bits("b011001")
  val FCMD_MFTX =       Bits("b011100")
  val FCMD_MFFSR =      Bits("b011101")
  val FCMD_MXTF =       Bits("b011110")
  val FCMD_MTFSR =      Bits("b011111")
  val FCMD_MADD =       Bits("b100100")
  val FCMD_MSUB =       Bits("b100101")
  val FCMD_NMSUB =      Bits("b100110")
  val FCMD_NMADD =      Bits("b100111")
  val FCMD_LOAD =       Bits("b111000")
  val FCMD_STORE =      Bits("b111001")
  val FCMD_WIDTH = 6
  val FSR_WIDTH = 8
}
import rocketFPConstants._

class rocketFPUCtrlSigs extends Bundle
{
  val cmd = Bits(width = FCMD_WIDTH)
  val valid = Bool()
  val wen = Bool()
  val ren1 = Bool()
  val ren2 = Bool()
  val ren3 = Bool()
  val single = Bool()
  val fromint = Bool()
  val toint = Bool()
  val store = Bool()
  val fsr = Bool()
}

class rocketFPUDecoder extends Component
{
  val io = new Bundle {
    val inst = Bits(32, INPUT)
    val sigs = new rocketFPUCtrlSigs().asOutput
  }
//  val fp =
//  ListLookup(io.dpath.inst,
//    List(FPU_N, FPU_N, FPU_N, FPU_N, FPU_N),
//     Array(
//        FMOVZ -> List(Bool(true)),
//        FMOVN -> List(Bool(true)),
//        FADD_S -> List(Bool(true)),
//        FSUB_S -> List(Bool(true)),
//        FMUL_S -> List(Bool(true)),
//        FDIV_S -> List(Bool(true)),
//        FSQRT_S -> List(Bool(true)),
//        FSGNJ_S -> List(Bool(true)),
//        FSGNJN_S -> List(Bool(true)),
//        FSGNJX_S -> List(Bool(true)),
//        FADD_D -> List(Bool(true)),
//        FSUB_D -> List(Bool(true)),
//        FMUL_D -> List(Bool(true)),
//        FDIV_D -> List(Bool(true)),
//        FSQRT_D -> List(Bool(true)),
//        FSGNJ_D -> List(Bool(true)),
//        FSGNJN_D -> List(Bool(true)),
//        FSGNJX_D -> List(Bool(true)),
//        FCVT_L_S -> List(Bool(true)),
//        FCVT_LU_S -> List(Bool(true)),
//        FCVT_W_S -> List(Bool(true)),
//        FCVT_WU_S -> List(Bool(true)),
//        FCVT_L_D -> List(Bool(true)),
//        FCVT_LU_D -> List(Bool(true)),
//        FCVT_W_D -> List(Bool(true)),
//        FCVT_WU_D -> List(Bool(true)),
//        FCVT_S_L -> List(Bool(true)),
//        FCVT_S_LU -> List(Bool(true)),
//        FCVT_S_W -> List(Bool(true)),
//        FCVT_S_WU -> List(Bool(true)),
//        FCVT_D_L -> List(Bool(true)),
//        FCVT_D_LU -> List(Bool(true)),
//        FCVT_D_W -> List(Bool(true)),
//        FCVT_D_WU -> List(Bool(true)),
//        FCVT_S_D -> List(Bool(true)),
//        FCVT_D_S -> List(Bool(true)),
//        FEQ_S -> List(Bool(true)),
//        FLT_S -> List(Bool(true)),
//        FLE_S -> List(Bool(true)),
//        FEQ_D -> List(Bool(true)),
//        FLT_D -> List(Bool(true)),
//        FLE_D -> List(Bool(true)),
//        FMIN_S -> List(Bool(true)),
//        FMAX_S -> List(Bool(true)),
//        FMIN_D -> List(Bool(true)),
//        FMAX_D -> List(Bool(true)),
//        MFTX_S -> List(Bool(true)),
//        MFTX_D -> List(Bool(true)),
//        MFFSR -> List(Bool(true)),
//        MXTF_S -> List(Bool(true)),
//        MXTF_D -> List(Bool(true)),
//        MTFSR -> List(Bool(true)),
//        FLW -> List(FPU_Y, FPU_Y, FPU_N, FPU_N, FPU_N),
//        FLD -> List(FPU_Y, FPU_Y, FPU_N, FPU_N, FPU_N),
//        FSW -> List(FPU_Y, FPU_N, FPU_N, FPU_Y, FPU_N),
//        FSD -> List(FPU_Y, FPU_N, FPU_N, FPU_Y, FPU_N)
//        FMADD_S -> List(Bool(true)),
//        FMSUB_S -> List(Bool(true)),
//        FNMSUB_S -> List(Bool(true)),
//        FNMADD_S -> List(Bool(true)),
//        FMADD_D -> List(Bool(true)),
//        FMSUB_D -> List(Bool(true)),
//        FNMSUB_D -> List(Bool(true)),
//        FNMADD_D -> List(Bool(true))
//     ));

  val N = Bool(false)
  val Y = Bool(true)
  val X = Bool(false)
  val FCMD_X = FCMD_ADD
  val decoder = ListLookup(io.inst,
    List                  (N,FCMD_X,       X,X,X,X,X,X,X,X,X),
    Array(FLW      -> List(Y,FCMD_LOAD,    Y,N,N,N,Y,N,N,N,N),
          FLD      -> List(Y,FCMD_LOAD,    Y,N,N,N,N,N,N,N,N),
          FSW      -> List(Y,FCMD_STORE,   N,N,Y,N,Y,N,N,Y,N),
          FSD      -> List(Y,FCMD_STORE,   N,N,Y,N,N,N,N,Y,N),
          MXTF_S   -> List(Y,FCMD_MXTF,    Y,N,N,N,Y,Y,N,N,N),
          MXTF_D   -> List(Y,FCMD_MXTF,    Y,N,N,N,N,Y,N,N,N),
          MFTX_S   -> List(Y,FCMD_MFTX,    N,Y,N,N,Y,N,Y,N,N),
          MFTX_D   -> List(Y,FCMD_MFTX,    N,Y,N,N,N,N,Y,N,N),
          MTFSR    -> List(Y,FCMD_MTFSR,   N,N,N,N,Y,Y,Y,N,Y),
          MFFSR    -> List(Y,FCMD_MFFSR,   N,N,N,N,Y,N,Y,N,Y)
          ))
  val valid :: cmd :: wen :: ren1 :: ren2 :: ren3 :: single :: fromint :: toint :: store :: fsr :: Nil = decoder

  io.sigs.valid := valid.toBool
  io.sigs.cmd := cmd
  io.sigs.wen := wen.toBool
  io.sigs.ren1 := ren1.toBool
  io.sigs.ren2 := ren2.toBool
  io.sigs.ren3 := ren3.toBool
  io.sigs.single := single.toBool
  io.sigs.fromint := fromint.toBool
  io.sigs.toint := toint.toBool
  io.sigs.store := store.toBool
  io.sigs.fsr := fsr.toBool
}

class ioDpathFPU extends Bundle {
  val inst = Bits(32, OUTPUT)
  val fromint_data = Bits(64, OUTPUT)

  val store_data = Bits(64, INPUT)
  val toint_data = Bits(64, INPUT)

  val dmem_resp_val = Bool(OUTPUT)
  val dmem_resp_tag = UFix(5, OUTPUT)
  val dmem_resp_data = Bits(64, OUTPUT)
}

class ioCtrlFPU extends Bundle {
  val valid = Bool(OUTPUT)
  val nack = Bool(INPUT)
  val killx = Bool(OUTPUT)
  val killm = Bool(OUTPUT)
  val dec = new rocketFPUCtrlSigs().asInput
}

class rocketFPIntUnit extends Component
{
  val io = new Bundle {
    val single = Bool(INPUT)
    val cmd = Bits(FCMD_WIDTH, INPUT)
    val fsr = Bits(FSR_WIDTH, INPUT)
    val in = Bits(65, INPUT)
    val store_data = Bits(64, OUTPUT)
    val toint_data = Bits(64, OUTPUT)
    val exc = Bits(5, OUTPUT)
  }

  val unrecoded_s = io.in(31,0)
  val unrecoded_d = io.in

  io.store_data := Mux(io.single, Cat(unrecoded_s, unrecoded_s), unrecoded_d)

  val scmp = Bool(false)
  val scmp_exc = Bits(0)

  val s2i = UFix(0)
  val s2i_exc = Bits(0)

  val dcmp = Bool(false)
  val dcmp_exc = Bits(0)

  val d2i = UFix(0)
  val d2i_exc = Bits(0)

  // output muxing
  val (out_s, exc_s) = (Wire() { Bits() }, Wire() { Bits() })
  out_s := Cat(Fill(32, unrecoded_s(31)), unrecoded_s)
  exc_s := Bits(0)
  val (out_d, exc_d) = (Wire() { Bits() }, Wire() { Bits() })
  out_d := unrecoded_d
  exc_d := Bits(0)

  when (io.cmd === FCMD_MTFSR || io.cmd === FCMD_MFFSR) {
    out_s := io.fsr
  }
  when (io.cmd === FCMD_CVT_W_FMT || io.cmd === FCMD_CVT_WU_FMT) {
    out_s := Cat(Fill(32, s2i(31)), s2i(31,0))
    exc_s := s2i_exc
    out_d := Cat(Fill(32, d2i(31)), d2i(31,0))
    exc_d := d2i_exc
  }
  when (io.cmd === FCMD_CVT_L_FMT || io.cmd === FCMD_CVT_LU_FMT) {
    out_s := s2i
    exc_s := s2i_exc
    out_d := d2i
    exc_d := d2i_exc
  }
  when (io.cmd === FCMD_EQ || io.cmd === FCMD_LT || io.cmd === FCMD_LE) {
    out_s := scmp
    exc_s := scmp_exc
    out_d := dcmp
    exc_d := dcmp_exc
  }

  io.toint_data := Mux(io.single, out_s, out_d)
  io.exc := Mux(io.single, exc_s, exc_d)
}

class rocketFPU extends Component
{
  val io = new Bundle {
    val ctrl = new ioCtrlFPU().flip()
    val dpath = new ioDpathFPU().flip()
  }

  val reg_inst = Reg() { Bits() }
  when (io.ctrl.valid) {
    reg_inst := io.dpath.inst
  }
  val reg_valid = Reg(io.ctrl.valid, Bool(false))

  val fp_decoder = new rocketFPUDecoder
  fp_decoder.io.inst := io.dpath.inst

  val ctrl = Reg() { new rocketFPUCtrlSigs }
  when (io.ctrl.valid) {
    ctrl := fp_decoder.io.sigs
  }

  // load response
  val load_wb = Reg(io.dpath.dmem_resp_val, resetVal = Bool(false))
  val load_wb_data = Reg() { Bits(width = 64) } // XXX WTF why doesn't bit width inference work for the regfile?!
  val load_wb_tag = Reg() { UFix() }
  when (io.dpath.dmem_resp_val) {
    load_wb_data := io.dpath.dmem_resp_data
    load_wb_tag := io.dpath.dmem_resp_tag
  }

  val fsr_rm = Reg() { Bits(width = 3) }
  val fsr_exc = Reg() { Bits(width = 5) }

  // regfile
  val regfile = Mem(32, load_wb, load_wb_tag, load_wb_data);
  regfile.setReadLatency(0);
  regfile.setTarget('inst);

  val ex_rs1 = regfile.read(reg_inst(26,22))
  val ex_rs2 = regfile.read(reg_inst(21,17))
  val ex_rs3 = regfile.read(reg_inst(16,12))

  val fp_fromint_val = Reg(resetVal = Bool(false))
  val fp_fromint_data = Reg() { Bits() }
  val fp_toint_val = Reg(resetVal = Bool(false))
  val fp_toint_data = Reg() { Bits() }
  val fp_toint_single = Reg() { Bool() }
  val fp_toint_cmd = Reg() { Bits() }
  val fp_waddr = Reg() { Bits() }

  fp_fromint_val := Bool(false)
  fp_toint_val := Bool(false)
  when (reg_valid && !io.ctrl.killx) {
    fp_waddr := reg_inst(31,27)
    when (ctrl.fromint) {
      fp_fromint_val := Bool(true)
      fp_fromint_data := io.dpath.fromint_data
    }
    when (ctrl.toint) {
      fp_toint_val := Bool(true)
      fp_toint_data := ex_rs1
    }
    when (ctrl.store) {
      fp_toint_data := ex_rs2
    }
    when (ctrl.toint || ctrl.store) {
      fp_toint_single := ctrl.single
      fp_toint_cmd := ctrl.cmd
    }
  }

  // currently we assume FP stores and FP->int ops take 1 cycle (MEM)
  val fpiu = new rocketFPIntUnit
  fpiu.io.single := ctrl.single
  fpiu.io.cmd := ctrl.cmd
  fpiu.io.fsr := Cat(fsr_rm, fsr_exc)
  fpiu.io.in := fp_toint_data

  io.dpath.store_data := fpiu.io.store_data
  io.dpath.toint_data := fpiu.io.toint_data

  val retire_toint = Reg(!io.ctrl.killm && fp_toint_val, resetVal = Bool(false))
  val retire_toint_exc = Reg(fpiu.io.exc)
  val retire_fromint = Reg(!io.ctrl.killm && fp_fromint_val, resetVal = Bool(false))
  val retire_fromint_wdata = Reg(fp_fromint_data)
  val retire_fromint_waddr = Reg(fp_waddr)

  when (retire_toint) {
    fsr_exc := fsr_exc | retire_toint_exc
  }
  when (retire_toint && retire_fromint) { // MTFSR
    fsr_exc := retire_fromint_wdata(4,0)
    fsr_rm := retire_fromint_wdata(7,5)
  }

  regfile.write(retire_fromint_waddr, retire_fromint_wdata, retire_fromint && !retire_toint)

  val fp_inflight = fp_toint_val || retire_toint || fp_fromint_val || retire_fromint
  val mtfsr_inflight = fp_toint_val && fp_fromint_val || retire_toint && retire_fromint
  val fsr_busy = ctrl.fsr && fp_inflight || mtfsr_inflight
  val units_busy = Bool(false)
  val write_port_busy = Bool(false)
  io.ctrl.nack := fsr_busy || units_busy || write_port_busy
  io.ctrl.dec <> fp_decoder.io.sigs
}
