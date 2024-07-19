package aslloader

import util.Logger
import ir._
import analysis.BitVectorEval._
import collection.mutable.ArrayBuffer
import collection.mutable

//type RTSym = Expr
//type RTLabel = String
//type BV = BitVecLiteral

import ir.dsl._


object Counter:
  var counter = 0

class Mutable[T](var v: T)

trait LiftState[RTSym, RTLabel, BV <: RTSym] {

  /* Lift-time semantics */
  def mkBits(n: BigInt, y: BigInt): BV
  def bvextract(e: BV, lo: BigInt, width: BigInt): BV
  def f_eq_bits(t: BigInt, x: BV, y: BV): Boolean
  def f_ne_bits(t: BigInt, x: BV, y: BV): Boolean
  def f_add_bits(t: BigInt, x: BV, y: BV): BV
  def f_sub_bits(t: BigInt, x: BV, y: BV): BV
  def f_mul_bits(t: BigInt, x: BV, y: BV): BV
  def f_and_bits(t: BigInt, x: BV, y: BV): BV
  def f_or_bits(t: BigInt, x: BV, y: BV): BV
  def f_eor_bits(t: BigInt, x: BV, y: BV): BV
  def f_not_bits(t: BigInt, x: BV): BV
  def f_slt_bits(t: BigInt, x: BV, y: BV): Boolean
  def f_sle_bits(t: BigInt, x: BV, y: BV): Boolean
  def f_zeros_bits(w: BigInt): BV
  def f_ones_bits(w: BigInt): BV
  def f_ZeroExtend(t0: BigInt, t1: BigInt, n: BV, x: BigInt): BV
  def f_SignExtend(t0: BigInt, t1: BigInt, n: BV, x: BigInt): BV
  def f_asr_bits(targ0: BigInt, targ1: BigInt, arg0: BV, arg1: BV): BV
  def f_lsl_bits(targ0: BigInt, targ1: BigInt, arg0: BV, arg1: BV): BV
  def f_lsr_bits(targ0: BigInt, targ1: BigInt, arg0: BV, arg1: BV): BV
  def f_decl_bool(arg0: String): RTSym
  def f_decl_bv(arg0: String, arg1: BigInt): RTSym
  def f_AtomicEnd(): RTSym
  def f_AtomicStart(): RTSym

  def f_replicate_bits(targ0: BigInt, targ1: BigInt, arg0: BV, arg1: BigInt): BV
  def f_append_bits(targ0: BigInt, targ1: BigInt, a: BV, b: BV): BV

  /** Run-time IR program generation */

  def f_gen_BFAdd(arg0: RTSym, arg1: RTSym): RTSym
  def f_gen_BFMul(arg0: RTSym, arg1: RTSym): RTSym
  def f_gen_FPAdd(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym
  def f_gen_FPCompare(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym, arg3: RTSym): RTSym
  def f_gen_FPCompareEQ(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym
  def f_gen_FPCompareGE(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym
  def f_gen_FPCompareGT(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym
  def f_gen_FPConvert(targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym
  def f_gen_FPConvertBF(arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym
  def f_gen_FPDiv(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym
  def f_gen_FPMax(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym
  def f_gen_FPMaxNum(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym
  def f_gen_FPMin(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym
  def f_gen_FPMinNum(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym
  def f_gen_FPMul(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym
  def f_gen_FPMulAdd(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym, arg3: RTSym): RTSym
  def f_gen_FPMulAddH(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym, arg3: RTSym): RTSym
  def f_gen_FPMulX(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym
  def f_gen_FPRSqrtStepFused(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym
  def f_gen_FPRecipEstimate(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym
  def f_gen_FPRecipStepFused(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym
  def f_gen_FPRecpX(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym
  def f_gen_FPRoundInt(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym, arg3: RTSym): RTSym
  def f_gen_FPRoundIntN(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym, arg3: RTSym): RTSym
  def f_gen_FPSqrt(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym
  def f_gen_FPSub(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym
  def f_gen_FPToFixed(targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym, arg3: RTSym, arg4: RTSym): RTSym
  def f_gen_FPToFixedJS_impl(targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym
  def f_gen_FixedToFP(targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym, arg3: RTSym, arg4: RTSym): RTSym
  def f_gen_bit_lit(targ0: BigInt, arg0: BV): RTSym
  def f_gen_bool_lit(arg0: Boolean): RTSym
  def f_gen_branch(arg0: RTSym): RTLabel
  def f_cvt_bits_uint(targ0: BigInt, arg0: BV): BigInt
  def f_gen_cvt_bits_uint(targ0: BigInt, arg0: RTSym): RTSym
  def f_gen_cvt_bool_bv(arg0: RTSym): RTSym
  def f_gen_eor_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym
  def f_gen_eq_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym
  def f_gen_eq_enum(arg0: RTSym, arg1: RTSym): RTSym
  def f_gen_int_lit(arg0: BigInt): BV
  def f_gen_store(lval: RTSym, e: RTSym): Unit
  def f_gen_load(e: RTSym): RTSym
  def f_gen_SignExtend(targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: BV): RTSym
  def f_gen_ZeroExtend(targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: BV): RTSym
  def f_gen_add_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym
  def f_gen_and_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym
  def f_gen_and_bool(arg0: RTSym, arg1: RTSym): RTSym
  def f_gen_asr_bits(targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: RTSym): RTSym
  def f_gen_lsl_bits(targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: RTSym): RTSym
  def f_gen_lsr_bits(targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: RTSym): RTSym
  def f_gen_mul_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym
  def f_gen_ne_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym
  def f_gen_not_bits(targ0: BigInt, arg0: RTSym): RTSym
  def f_gen_not_bool(arg0: RTSym): RTSym
  def f_gen_or_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym
  def f_gen_or_bool(arg0: RTSym, arg1: RTSym): RTSym
  def f_gen_sdiv_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym
  def f_gen_sle_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym
  def f_gen_slt_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym
  def f_gen_sub_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym
  def f_gen_AArch64_MemTag_set(arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym
  def f_gen_Mem_read(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym
  def f_gen_slice(e: RTSym, lo: BigInt, wd: BigInt): RTSym
  def f_gen_replicate_bits(targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: BV): RTSym
  def f_gen_append_bits(targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: RTSym): RTSym
  def f_gen_array_load(arg0: RTSym, arg1: BigInt): RTSym
  def f_gen_array_store(arg0: RTSym, arg1: BigInt, arg2: RTSym): Unit
  def f_gen_Mem_set(sz: BigInt, ptr: RTSym, width: BV, acctype: RTSym, value: RTSym): Unit
  def f_gen_assert(arg0: RTSym): Unit

  def f_switch_context(arg0: RTLabel): Unit
  def f_true_branch(arg0: RTLabel): RTLabel
  def f_false_branch(arg0: RTLabel): RTLabel
  def f_merge_branch(arg0: RTLabel): RTLabel


  /** Global variable definitions * */

  def rTLabelDefault : RTLabel
  def rTSymDefault : RTSym
  def rTExprDefault : RTSym

  def v_PSTATE_UAO: Mutable[RTSym]
  def v_PSTATE_PAN: Mutable[RTSym]
  def v_PSTATE_DIT: Mutable[RTSym]
  def v_PSTATE_SSBS: Mutable[RTSym]
  def v_PSTATE_G: Mutable[RTSym]
  def v_PSTATE_A: Mutable[RTSym]
  def v_PSTATE_I: Mutable[RTSym]
  def v_PSTATE_F: Mutable[RTSym]
  def v_PSTATE_D: Mutable[RTSym]
  def v_PSTATE_C: Mutable[RTSym]
  def v_PSTATE_Z: Mutable[RTSym]
  def v_PSTATE_V: Mutable[RTSym]
  def v_PSTATE_N: Mutable[RTSym]
  def v__PC: Mutable[RTSym]
  def v__R: Mutable[RTSym]
  def v__Z: Mutable[RTSym]
  def v_SP_EL0: Mutable[RTSym]
  def v_FPSR: Mutable[RTSym]
  def v_FPCR: Mutable[RTSym]

  def v_PSTATE_BTYPE: Mutable[RTSym]
  def v_BTypeCompatible: Mutable[RTSym]
  def v___BranchTaken: Mutable[RTSym]
  def v_BTypeNext: Mutable[RTSym]
  def v___ExclusiveLocal: Mutable[RTSym]



}

case class BranchInfo(val branch: Option[String], val guard: Expr, val branchTaken: Boolean, pcAssigned: Option[Expr])

class BASILLiftState(val entry: String = "block") extends LiftState[Expr, String, BitVecLiteral] {

  type RTSym = Expr
  type RTLabel = String
  type BV = BitVecLiteral 

  def rTLabelDefault : RTLabel = "undefined"
  def rTSymDefault : RTSym = null
  def rTExprDefault : RTSym = null

  /** Constant variable definitions */
  def v_PSTATE_UAO = Mutable(Register("UAO", BitVecType(1))) // Expr_Field(Expr_Var(Ident "PSTATE"), Ident "G")
  def v_PSTATE_PAN = Mutable(Register("PAN", BitVecType(1))) // Expr_Field(Expr_Var(Ident "PSTATE"), Ident "G")
  def v_PSTATE_DIT = Mutable(Register("DIT", BitVecType(1))) // Expr_Field(Expr_Var(Ident "PSTATE"), Ident "G")
  def v_PSTATE_SSBS = Mutable(Register("SSBS", BitVecType(1))) // Expr_Field(Expr_Var(Ident "PSTATE"), Ident "G")
  def v_PSTATE_G = Mutable(Register("G", BitVecType(1))) // Expr_Field(Expr_Var(Ident "PSTATE"), Ident "G")
  def v_PSTATE_A = Mutable(Register("A", BitVecType(1))) // Expr_Field(Expr_Var(Ident "PSTATE"), Ident "G")
  def v_PSTATE_I = Mutable(Register("I", BitVecType(1))) // Expr_Field(Expr_Var(Ident "PSTATE"), Ident "G")
  def v_PSTATE_F = Mutable(Register("F", BitVecType(1))) // Expr_Field(Expr_Var(Ident "PSTATE"), Ident "G")
  def v_PSTATE_D = Mutable(Register("D", BitVecType(1))) // Expr_Field(Expr_Var(Ident "PSTATE"), Ident "G")
  def v_PSTATE_C = Mutable(Register("CF", BitVecType(1))) // Expr_Field(Expr_Var(Ident "PSTATE"), Ident "C")
  def v_PSTATE_Z = Mutable(Register("ZF", BitVecType(1))) // Expr_Field(Expr_Var(Ident "PSTATE"), Ident "Z")
  def v_PSTATE_V = Mutable(Register("VF", BitVecType(1))) // Expr_Field(Expr_Var(Ident "PSTATE"), Ident "V")
  def v_PSTATE_N = Mutable(Register("NF", BitVecType(1))) // Expr_Field(Expr_Var(Ident "PSTATE"), Ident "N")
  def v__PC = Mutable(Register("_PC", BitVecType(64))) // Expr_Var(Ident "_PC")
  def v__R = Mutable(Register("_R", MapType(BitVecType(64),BitVecType(64))))
  def v__Z = Mutable(Register("_Z", MapType(BitVecType(64),BitVecType(128))))
  def v_SP_EL0 = Mutable(Register("R31", BitVecType(64)))
  def v_FPSR = Mutable(Register("FPSR", BoolType))
  def v_FPCR = Mutable(LocalVar("FPCR", BitVecType(32)))

  def v_PSTATE_BTYPE = Mutable(Register("PSTATE.BTYPE", BoolType)) // Expr_Field(Expr_Var(Ident "PSTATE"), Ident "BTYPE")
  def v_BTypeCompatible = Mutable(Register("BTypeCompatible", BoolType)) // Expr_Var (Ident "BTypeCompatible")
  def v___BranchTaken = Mutable(Register("BranchTaken", BoolType))
  def v_BTypeNext = Mutable(Register("BTypeNext", BoolType))
  def v___ExclusiveLocal = Mutable(Register("__ExclusiveLocal", BoolType))

  val endian = Endian.LittleEndian
  val memory = Memory("mem", 64, 8)

  var current_pos: String = entry


  val controlFlow: mutable.Map[String, EventuallyJump] = mutable.Map()
  val blocks: mutable.LinkedHashMap[String, ArrayBuffer[Statement]] = mutable.LinkedHashMap((entry -> ArrayBuffer.empty))
  val branches: mutable.Map[String, (String, String, String)] = mutable.Map()


  var current_guard: BranchInfo = BranchInfo(None, TrueLiteral, false, None)

  // maps block ids to guards
  // We push the current guard forward as blocks are appended. We maintain this mapping so we 
  // can update the current guard when switch_ctx is called. 
  val block_guard: mutable.Map[String, BranchInfo] = mutable.Map()

  def pcAssigns = block_guard.filter {
    case (k,v) => v.pcAssigned.isDefined
  } 

  def new_name(p: Option[String] = None) = {
    Counter.counter += 1
    entry + "_" + p.map(_ + "_").getOrElse("") + Counter.counter
  }

  def merge_state(other: BASILLiftState) = {
    controlFlow.addAll(other.controlFlow)
    blocks.addAll(other.blocks)
    branches.addAll(other.branches)
    current_pos = other.current_pos
  }

  def escaping_jumps = controlFlow
    .collect {
      case (_, EventuallyGoto(tgts))                => tgts.map((t: DelayNameResolve) => t.ident)
      case (_, EventuallyCall(t, Some(ft)))         => List(ft.ident)
      case (_, EventuallyIndirectCall(_, Some(ft))) => List(ft.ident)
    }
    .flatMap(_.toList)
    .filter((t) => !blocks.keySet.contains(t))
    .toSet

  def push_block(p: Option[String] = None): String = {
    val n = new_name(p)

    blocks(n) = ArrayBuffer.empty
    n
  }

  def implicit_set_pc(address: Long, label: Option[String] = None) = {
    val la = LocalAssign(Register("_PC", BitVecType(64)), BitVecLiteral(BigInt(address), 64), label)
    blocks(current_pos).append(la)
  }

  def push_stmt(s: Statement) = {
    s match {
      case LocalAssign(Register("BranchTaken", BoolType), TrueLiteral, _) => current_guard = BranchInfo(current_guard.branch, current_guard.guard, true, current_guard.pcAssigned) 
      case LocalAssign(Register("_PC", BitVecType(64)), addr, _) => current_guard = BranchInfo(current_guard.branch, current_guard.guard, current_guard.branchTaken, Some(addr))
      case _ => ()
    }

    block_guard(current_pos) = current_guard
    blocks(current_pos).append(s)
  }

  def switch_ctx(c: String) = {
    require(blocks.keySet.contains(c))
    current_pos = c
    current_guard = block_guard(current_pos) 
  }

  def gen_branch(cond: Expr) = {
    val branch_id = new_name(Some("branch"))

    val true_branch = push_block(Some("true"))
    val false_branch = push_block(Some("false"))
    val merge_block = push_block(Some("join"))
    blocks(true_branch).append(Assume(cond))
    blocks(false_branch).append(Assume(UnaryExpr(BoolNOT, cond)))

    block_guard(true_branch) = BranchInfo(Some(branch_id), cond, false, None)
    block_guard(false_branch) = BranchInfo(Some(branch_id), UnaryExpr(BoolNOT, cond), false, None)
    block_guard(merge_block) = BranchInfo(None, TrueLiteral, false, None)
    current_guard = BranchInfo(Some(branch_id), current_guard.guard, current_guard.branchTaken, current_guard.pcAssigned)

    controlFlow(current_pos) = goto(true_branch, false_branch)
    controlFlow(true_branch) = goto(merge_block)
    controlFlow(false_branch) = goto(merge_block)
    branches.addOne((branch_id -> (true_branch, false_branch, merge_block)))
    switch_ctx (merge_block)
    (branch_id, true_branch, false_branch, merge_block)
  }

  def replace_jmp(c: EventuallyJump) = controlFlow(current_pos) = c

  def add_call(from: String, c: EventuallyJump) : Unit = {
    controlFlow.get(from) match {
      case None => controlFlow(current_pos) = c
      case Some(EventuallyGoto(List(x))) => {
        c match {
          case EventuallyCall(c, None) => EventuallyCall(c, Some(x))
          case EventuallyCall(_, Some(f)) => add_call(f.ident, c)
          case EventuallyIndirectCall(c, None) => EventuallyIndirectCall(c, Some(x))
          case EventuallyIndirectCall(_, Some(f)) => add_call(f.ident, c)
          case EventuallyGoto(cs) => (EventuallyGoto(cs ++ List(x)))
          case _ => throw Exception(s"Existing jump ${EventuallyGoto(List(x))} adding $c")
        }

      }
      case Some(l) => throw Exception(s"Existing jump $l")
    }
  }

  def add_call(c: EventuallyJump) : Unit = {
    add_call(current_pos, c)
  }

  def add_goto(l: String) = {
    controlFlow.get(current_pos) match {
      case Some(EventuallyGoto(ts)) => controlFlow(current_pos) = EventuallyGoto(ts ++ List(DelayNameResolve(l)))
      case Some(EventuallyCall(ts, None)) => controlFlow(current_pos) = EventuallyCall(ts, Some(DelayNameResolve(l)))
      case None                     => controlFlow(current_pos) = EventuallyGoto(List(DelayNameResolve(l)))
      case Some(l)                  => throw RuntimeException(s"Cannot add goto target to call $l")
    }
  }

  def toIR(): List[EventuallyBlock] =
    blocks.map((n, stmts) => block(n, (stmts.toList ++ List(controlFlow.getOrElse(n, ret))))).toList

  def extract(x: BigInt, sz: BigInt) = x % (BigInt(2).pow((sz + 1).toInt))

  def mkBits(n: BigInt, y: BigInt): BitVecLiteral = {
    require(n >= 0)
    BitVecLiteral(extract(y, n), n.toInt)
  }

  def zero_extend_to(s: BigInt, x: BitVecLiteral) = {
    require(s > x.size)
    BitVecLiteral(x.value, s.toInt)
  }

  def gen_zero_extend_to(s: BigInt, x: Expr) = {
    x.getType match {
      case BitVecType(sz) if sz == s.toInt => x 
      case BitVecType(sz) => ZeroExtend((s - sz).toInt, x)
      case _              => throw Exception("Type mismatch gen_zero_extend_to")
    }
  }

  def bvextract(e: BitVecLiteral, lo: BigInt, width: BigInt): BitVecLiteral =
    smt_extract((lo + width - 1).toInt, lo.toInt, e)

  def f_eq_bits(t: BigInt, x: BitVecLiteral, y: BitVecLiteral): Boolean = (smt_bveq(x, y) == TrueLiteral)

  def f_ne_bits(t: BigInt, x: BitVecLiteral, y: BitVecLiteral): Boolean = (smt_bveq(x, y) == FalseLiteral)

  def f_add_bits(t: BigInt, x: BitVecLiteral, y: BitVecLiteral): BitVecLiteral = (smt_bvadd(x, y))

  def f_sub_bits(t: BigInt, x: BitVecLiteral, y: BitVecLiteral): BitVecLiteral = (smt_bvsub(x, y))

  def f_mul_bits(t: BigInt, x: BitVecLiteral, y: BitVecLiteral): BitVecLiteral = (smt_bvmul(x, y))

  def f_and_bits(t: BigInt, x: BitVecLiteral, y: BitVecLiteral): BitVecLiteral = (smt_bvand(x, y))

  def f_or_bits(t: BigInt, x: BitVecLiteral, y: BitVecLiteral): BitVecLiteral = (smt_bvor(x, y))

  def f_eor_bits(t: BigInt, x: BitVecLiteral, y: BitVecLiteral): BitVecLiteral = (smt_bvxor(x, y))

  def f_not_bits(t: BigInt, x: BitVecLiteral): BitVecLiteral = (smt_bvnot(x))

  def f_slt_bits(t: BigInt, x: BitVecLiteral, y: BitVecLiteral): Boolean =
    (TrueLiteral == (smt_bvslt(x, y)))

  def f_sle_bits(t: BigInt, x: BitVecLiteral, y: BitVecLiteral): Boolean =
    (TrueLiteral == (smt_bvsle(x, y)))

  def f_zeros_bits(w: BigInt): BitVecLiteral = BitVecLiteral(0, w.toInt)

  def f_ones_bits(w: BigInt): BitVecLiteral = BitVecLiteral(BigInt(2).pow(w.toInt) - 1, w.toInt)

  def f_ZeroExtend(t0: BigInt, t1: BigInt, n: BitVecLiteral, x: BigInt): BitVecLiteral =
    smt_zero_extend(x.toInt - n.size, n)

  def f_SignExtend(t0: BigInt, t1: BigInt, n: BitVecLiteral, x: BigInt): BitVecLiteral =
    smt_sign_extend(x.toInt - n.size, n)

  def f_asr_bits(targ0: BigInt, targ1: BigInt, arg0: BitVecLiteral, arg1: BitVecLiteral): BitVecLiteral =
    smt_bvashr(arg0, arg1)

  def f_lsl_bits(targ0: BigInt, targ1: BigInt, arg0: BitVecLiteral, arg1: BitVecLiteral): BitVecLiteral =
    smt_bvshl(arg0, zero_extend_to(arg0.size, arg1))

  def f_lsr_bits(targ0: BigInt, targ1: BigInt, arg0: BitVecLiteral, arg1: BitVecLiteral): BitVecLiteral =
    smt_bvlshr(arg0, zero_extend_to(arg0.size, arg1))

  def f_decl_bool(arg0: String): RTSym = LocalVar(arg0, BoolType)
  def f_decl_bv(arg0: String, arg1: BigInt): RTSym = LocalVar(arg0, BitVecType(arg1.toInt))

  def f_gen_BFAdd(arg0: RTSym, arg1: RTSym): RTSym = throw NotImplementedError("func not implemented")
  def f_gen_BFMul(arg0: RTSym, arg1: RTSym): RTSym = throw NotImplementedError("func not implemented")

  def f_gen_FPAdd(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = throw NotImplementedError(
    "func not implemented"
  )
  def f_gen_FPCompare(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym, arg3: RTSym): RTSym =
    throw NotImplementedError("func not implemented")
  def f_gen_FPCompareEQ(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym =
    throw NotImplementedError("func not implemented")
  def f_gen_FPCompareGE(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym =
    throw NotImplementedError("func not implemented")
  def f_gen_FPCompareGT(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym =
    throw NotImplementedError("func not implemented")
  def f_gen_FPConvert(targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym =
    throw NotImplementedError("func not implemented")
  def f_gen_FPConvertBF(arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = throw NotImplementedError(
    "func not implemented"
  )
  def f_gen_FPDiv(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = throw NotImplementedError(
    "func not implemented"
  )
  def f_gen_FPMax(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = throw NotImplementedError(
    "func not implemented"
  )
  def f_gen_FPMaxNum(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym =
    throw NotImplementedError("func not implemented")
  def f_gen_FPMin(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = throw NotImplementedError(
    "func not implemented"
  )
  def f_gen_FPMinNum(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym =
    throw NotImplementedError("func not implemented")
  def f_gen_FPMul(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = throw NotImplementedError(
    "func not implemented"
  )
  def f_gen_FPMulAdd(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym, arg3: RTSym): RTSym =
    throw NotImplementedError("func not implemented")
  def f_gen_FPMulAddH(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym, arg3: RTSym): RTSym =
    throw NotImplementedError("func not implemented")
  def f_gen_FPMulX(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym =
    throw NotImplementedError("func not implemented")
  def f_gen_FPRSqrtStepFused(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = throw NotImplementedError(
    "func not implemented"
  )
  def f_gen_FPRecipEstimate(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = throw NotImplementedError(
    "func not implemented"
  )
  def f_gen_FPRecipStepFused(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = throw NotImplementedError(
    "func not implemented"
  )
  def f_gen_FPRecpX(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = throw NotImplementedError(
    "func not implemented"
  )
  def f_gen_FPRoundInt(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym, arg3: RTSym): RTSym =
    throw NotImplementedError("func not implemented")
  def f_gen_FPRoundIntN(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym, arg3: RTSym): RTSym =
    throw NotImplementedError("func not implemented")
  def f_gen_FPSqrt(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = throw NotImplementedError(
    "func not implemented"
  )
  def f_gen_FPSub(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = throw NotImplementedError(
    "func not implemented"
  )
  def f_gen_FPToFixed(
      targ0: BigInt,
      targ1: BigInt,
      arg0: RTSym,
      arg1: RTSym,
      arg2: RTSym,
      arg3: RTSym,
      arg4: RTSym
  ): RTSym = throw NotImplementedError("func not implemented")
  def f_gen_FPToFixedJS_impl(targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym =
    throw NotImplementedError("func not implemented")
  def f_gen_FixedToFP(
      targ0: BigInt,
      targ1: BigInt,
      arg0: RTSym,
      arg1: RTSym,
      arg2: RTSym,
      arg3: RTSym,
      arg4: RTSym
  ): RTSym = throw NotImplementedError("func not implemented")

  def f_gen_bit_lit(targ0: BigInt, arg0: BitVecLiteral): RTSym = BitVecLiteral(arg0.value, targ0.toInt)
  def f_gen_bool_lit(arg0: Boolean): RTSym = if arg0 then TrueLiteral else FalseLiteral

  def f_gen_branch(arg0: RTSym): RTLabel = gen_branch(arg0)._1
  def f_true_branch(arg0: RTLabel): RTLabel = (branches(arg0))._1
  def f_false_branch(arg0: RTLabel): RTLabel = (branches(arg0))._2
  def f_merge_branch(arg0: RTLabel): RTLabel = (branches(arg0))._3

  def f_cvt_bits_uint(targ0: BigInt, arg0: BitVecLiteral): BigInt = arg0.value
  def f_gen_cvt_bits_uint(targ0: BigInt, arg0: RTSym): RTSym = throw Exception("cvt bitsnot implemented")
  def f_gen_cvt_bool_bv(arg0: RTSym): RTSym = arg0 match {
      case b: BinaryExpr if b.op == BVEQ => BinaryExpr(BVCOMP, b.arg1, b.arg2)
      case _ => throw Exception(s"unhandled conversion from bool to bitvector: ${arg0}")
    }

  def f_gen_eor_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = BinaryExpr(BVXOR, arg0, arg1)
  def f_gen_eq_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym =  BinaryExpr(BVEQ, arg0, arg1)
  /*{
    (arg0.getType, arg1.getType) match {
      case (b:BitVecType, v:BitVecType) => BinaryExpr(BVEQ, arg0, arg1)
      case (b:BitVecType, BoolType) => BinaryExpr(BVEQ, BinaryExpr(BVEQ, BitVecLiteral(0, targ0.toInt), arg0), arg1)
      case (BoolType, b:BitVecType) => BinaryExpr(BVEQ, BinaryExpr(BVEQ, BitVecLiteral(0, targ0.toInt), arg1), arg0)
      case (b:BitVecType, IntType) => BinaryExpr(IntEQ, BinaryExpr(BVEQ, BitVecLiteral(0, targ0.toInt), arg0), arg1)
      case (IntType, b:BitVecType) => BinaryExpr(IntEQ, BinaryExpr(BVEQ, BitVecLiteral(0, targ0.toInt), arg1), arg0)
      case (BoolType, BoolType) =>  BinaryExpr(BoolEQ, arg0, arg1)
      case (IntType, IntType) =>  BinaryExpr(IntEQ, arg0, arg1)
    }
  }*/


  /*def coerceTo(typ: IRType, v: Expr) = {
    (typ, v.getType) match {
      case (a, b) if a == b => v
      case (BitVecType(a), BitVecType(b)) if a > b =>  ZeroExtend((a - b).toInt, v)
      case (BoolType, BitVecType(s)) => BinaryExpr(BVNEQ, BitVecLiteral(0, s), v)
      case (BitVecType(s), BoolType) = 
    }
  }*/

  def f_gen_eq_enum(arg0: RTSym, arg1: RTSym): RTSym = BinaryExpr(BVEQ, arg0, arg1)
  def f_gen_int_lit(arg0: BigInt): BitVecLiteral = BitVecLiteral(arg0, 1123)

  def f_gen_store(lval: RTSym, e: RTSym): Unit = lval match
    case v: Variable => push_stmt(LocalAssign(v, e))
    case m           => throw NotImplementedError(s"fail assign $m")

  def f_gen_load(e: RTSym): RTSym = e match
    case m: Memory => throw NotImplementedError()
    case _         => e

  def f_gen_SignExtend(targ0: BigInt, targ1: BigInt, arg0: Expr, arg1: BitVecLiteral): RTSym = {
    val oldSize = (targ0)
    val newSize = (targ1)
    if (arg1.value != newSize) {
      throw Exception()
    }
    SignExtend((newSize - oldSize).toInt, arg0)
  }

  def f_gen_ZeroExtend(targ0: BigInt, targ1: BigInt, arg0: Expr, arg1: BitVecLiteral): RTSym = {
    val oldSize = (targ0)
    val newSize = (targ1)
    if (arg1.value != newSize) {
      throw Exception()
    }
    if ((newSize - oldSize) == 0) then arg0 else ZeroExtend((newSize - oldSize).toInt, arg0)
  }

  def f_gen_add_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = BinaryExpr(BVADD, arg0, arg1)
  def f_gen_and_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = BinaryExpr(BVAND, arg0, arg1)
  def f_gen_and_bool(arg0: RTSym, arg1: RTSym): RTSym = BinaryExpr(BoolAND, arg0, arg1)

  def f_gen_asr_bits(targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: RTSym): RTSym =
    BinaryExpr(BVASHR, arg0, gen_zero_extend_to(targ0, arg1))
  def f_gen_lsl_bits(targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: RTSym): RTSym =
    BinaryExpr(BVSHL, arg0, gen_zero_extend_to(targ0, arg1))
  def f_gen_lsr_bits(targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: RTSym): RTSym =
    BinaryExpr(BVLSHR, arg0, gen_zero_extend_to(targ0, arg1))
  def f_gen_mul_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = BinaryExpr(BVMUL, arg0, arg1)
  def f_gen_ne_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = BinaryExpr(BVCOMP, arg0, arg1)
  def f_gen_not_bits(targ0: BigInt, arg0: RTSym): RTSym = arg0.getType match {
    case BoolType       => UnaryExpr(BoolNOT, arg0)
    case BitVecType(_) => UnaryExpr(BVNOT, arg0)
    case _: MapType     => throw IllegalArgumentException()
    case IntType        => throw IllegalArgumentException()
  }

  def f_gen_not_bool(arg0: RTSym): RTSym = arg0.getType match {
    case BoolType       => UnaryExpr(BoolNOT, arg0)
    case BitVecType(sz) => BinaryExpr(BVNEQ, BitVecLiteral(0, sz), arg0)
    case _: MapType     => throw IllegalArgumentException()
    case IntType        => throw IllegalArgumentException()
  }

  def f_gen_or_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = BinaryExpr(BVOR, arg0, arg1)
  def f_gen_or_bool(arg0: RTSym, arg1: RTSym): RTSym = BinaryExpr(BoolOR, arg0, arg1)
  def f_gen_sdiv_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = BinaryExpr(BVSDIV, arg0, arg1)
  def f_gen_sle_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = BinaryExpr(BVSLE, arg0, arg1)
  def f_gen_slt_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = BinaryExpr(BVSLT, arg0, arg1)
  def f_gen_sub_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = BinaryExpr(BVSUB, arg0, arg1)

  def f_AtomicEnd(): RTSym = Register("ATOMICSTART", BoolType)
  def f_AtomicStart(): RTSym = Register("ATOMICSTART", BoolType)

  def f_replicate_bits(targ0: BigInt, targ1: BigInt, arg0: BitVecLiteral, arg1: BigInt): BitVecLiteral = {
    bv_replicate(arg0, arg1.toInt)
  }
  def f_append_bits(targ0: BigInt, targ1: BigInt, a: BitVecLiteral, b: BitVecLiteral): BitVecLiteral =
    BitVecLiteral((a.value << b.size) + b.value, (a.size + b.size))

  def f_gen_AArch64_MemTag_set(arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = throw NotImplementedError(
    "func not implemented"
  )

  def f_gen_Mem_set(sz: BigInt, ptr: RTSym, width: BitVecLiteral, acctype: RTSym, value: RTSym): Unit =
    assert(width.value == sz)
    val expr = MemoryStore(memory, ptr, value, endian, sz.toInt * memory.valueSize)
    val stmt = MemoryAssign(memory, expr)
    push_stmt(stmt)

  def f_gen_Mem_read(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym =
    MemoryLoad(memory, arg0, endian, targ0.toInt * memory.valueSize)

  def f_gen_slice(e: RTSym, lo: BigInt, wd: BigInt): RTSym = {
    Extract((wd + lo).toInt, lo.toInt, e)
  }
  def f_gen_replicate_bits(targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: BitVecLiteral): RTSym = {
    Range.Exclusive(1, arg1.value.toInt, 1).map(v => arg0).foldLeft(arg0)((a, b) => (BinaryExpr(BVCONCAT, a, b)))
  }
  def f_gen_append_bits(targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: RTSym): RTSym =
    BinaryExpr(BVCONCAT, arg0, arg1)

  def f_gen_array_load(arg0: RTSym, arg1: BigInt): RTSym = arg0 match
    case Register("_R", t) => Register("R" + arg1, BitVecType(64))
    case Register("_Z", t) => Register("V" + arg1, BitVecType(128))
    case _ => {
      Logger.warn(s"Unknown array load $arg0")
      arg0
    }
  def f_gen_array_store(arg0: RTSym, arg1: BigInt, arg2: RTSym): Unit = arg0 match
    case Register(n, t) if n.contains("R") => push_stmt(LocalAssign(Register("R" + arg1, BitVecType(64)), arg2))
    case _                                 => Logger.warn(s"Unknown array store $arg0")

  def f_gen_assert(arg0: RTSym) = push_stmt(Assert(arg0))
  def f_switch_context(arg0: RTLabel) = switch_ctx(arg0)



}

object Lifter {

  def liftOpcode(op: BigInt, sp: BigInt): List[EventuallyBlock] = {
    var liftState = BASILLiftState()
    val dec = f_A64_decoder[Expr, String, BitVecLiteral](liftState, BitVecLiteral(op, 32), BitVecLiteral(sp,64))
    liftState.toIR()
  }


  def liftOpcode(op: BigInt): List[EventuallyBlock] = {
    var liftState = BASILLiftState()
    val dec = f_A64_decoder[Expr, String, BitVecLiteral](liftState, BitVecLiteral(op, 32), BitVecLiteral(BigInt(0), 64))
    liftState.toIR()
  }

}


