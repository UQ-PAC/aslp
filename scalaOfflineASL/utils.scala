package aslloader

import util.Logger
import ir._
import analysis.BitVectorEval._
import collection.mutable.ArrayBuffer
import collection.mutable

type RTSym = Expr
type RTLabel = String
type BV = BitVecLiteral
def rTLabelDefault = "undef"
def rTSymDefault = null
def rTExprDefault = null

import ir.dsl._

class LiftState(val entry: String = "block") {

  val endian = Endian.LittleEndian
  val memory = Memory("mem", 64, 8)

  var counter = 0
  var current_pos: String = entry
  val controlFlow: mutable.Map[String, EventuallyJump] = mutable.Map()
  val blocks: mutable.Map[String, ArrayBuffer[Statement]] = mutable.Map((entry -> ArrayBuffer.empty))
  val branches: mutable.Map[String, (String, String, String)] = mutable.Map()

  def new_name(p: Option[String] = None) = entry + "_" + p.map(_ + "_").getOrElse("") + (counter += 1).toString

  def merge_state(other: LiftState) = {
    controlFlow.addAll(other.controlFlow)
    blocks.addAll(other.blocks)
    current_pos = other.current_pos
  }

  def escaping_jumps = controlFlow
    .collect {
      case (_, EventuallyGoto(tgts))                => tgts.map((t: DelayNameResolve) => t.ident)
      case (_, EventuallyCall(_, Some(ft)))         => List(ft.ident)
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

  def push_stmt(s: Statement) = {
    blocks(current_pos).append(s)
  }

  def switch_ctx(c: String) = {
    require(blocks.keySet.contains(c))
    current_pos = c
  }

  def gen_branch(cond: Expr): String = {
    val branch_id = new_name(Some("branch"))
    val true_branch = push_block(Some("true"))
    val false_branch = push_block(Some("false"))
    val merge_block = push_block(Some("join"))
    blocks(true_branch).append(Assume(cond))
    blocks(false_branch).append(Assume(UnaryExpr(BoolNOT, cond)))
    controlFlow(current_pos) = goto(true_branch, false_branch)
    controlFlow(true_branch) = goto(merge_block)
    controlFlow(true_branch) = goto(merge_block)
    branches.addOne((branch_id -> (true_branch, false_branch, merge_block)))
    branch_id
  }

  def add_call(c: EventuallyJump) = {
    controlFlow.get(current_pos) match {
      case None    => controlFlow(current_pos) = c
      case Some(l) => throw Exception(s"Existing jump $l")
    }
  }

  def add_goto(l: String) = {
    controlFlow.get(current_pos) match {
      case Some(EventuallyGoto(ts)) => controlFlow(current_pos) = EventuallyGoto(ts ++ List(DelayNameResolve(l)))
      case None                     => controlFlow(current_pos) = EventuallyGoto(List(DelayNameResolve(l)))
      case Some(l)                  => throw Exception(s"Cannot add goto target to call $l")
    }
  }

  def toIR(): List[EventuallyBlock] =
    blocks.map((n, stmts) => block(n, (stmts.toList ++ List(controlFlow.getOrElse(n, goto()))))).toList
}

object Lifter {

  def liftOpcode(op: Int): List[EventuallyBlock] = {
    var liftState = LiftState()
    val dec = A64_decoder()
    val r = dec.decode(liftState, BitVecLiteral(BigInt(op), 32))
    liftState.toIR()
  }

}

class Mutable[T](var v: T)

def extract(x: BigInt, sz: BigInt) = x % (BigInt(2).pow((sz + 1).toInt))

def mkBits(st: LiftState, n: BigInt, y: BigInt): BitVecLiteral = {
  require(n >= 0)
  BitVecLiteral(extract(y, n), n.toInt)
}

def zero_extend_to(s: BigInt, x: BitVecLiteral) = {
  require(s > x.size)
  BitVecLiteral(x.value, s.toInt)
}

def gen_zero_extend_to(s: BigInt, x: Expr) = {
  x.getType match {
    case BitVecType(sz) => ZeroExtend((s - sz).toInt, x)
    case _              => throw Exception("Type mismatch gen_zero_extend_to")
  }
}

def bvextract(st: LiftState, e: BitVecLiteral, lo: BigInt, width: BigInt): BitVecLiteral =
  smt_extract((lo + width - 1).toInt, lo.toInt, e)

def f_eq_bits(st: LiftState, t: BigInt, x: BitVecLiteral, y: BitVecLiteral): Boolean = (smt_bveq(x, y) == TrueLiteral)

def f_ne_bits(st: LiftState, t: BigInt, x: BitVecLiteral, y: BitVecLiteral): Boolean = (smt_bveq(x, y) == FalseLiteral)

def f_add_bits(st: LiftState, t: BigInt, x: BitVecLiteral, y: BitVecLiteral): BitVecLiteral = (smt_bvadd(x, y))

def f_sub_bits(st: LiftState, t: BigInt, x: BitVecLiteral, y: BitVecLiteral): BitVecLiteral = (smt_bvsub(x, y))

def f_mul_bits(st: LiftState, t: BigInt, x: BitVecLiteral, y: BitVecLiteral): BitVecLiteral = (smt_bvmul(x, y))

def f_and_bits(st: LiftState, t: BigInt, x: BitVecLiteral, y: BitVecLiteral): BitVecLiteral = (smt_bvand(x, y))

def f_or_bits(st: LiftState, t: BigInt, x: BitVecLiteral, y: BitVecLiteral): BitVecLiteral = (smt_bvor(x, y))

def f_eor_bits(st: LiftState, t: BigInt, x: BitVecLiteral, y: BitVecLiteral): BitVecLiteral = (smt_bvxor(x, y))

def f_not_bits(st: LiftState, t: BigInt, x: BitVecLiteral): BitVecLiteral = (smt_bvnot(x))

def f_slt_bits(st: LiftState, t: BigInt, x: BitVecLiteral, y: BitVecLiteral): Boolean =
  (TrueLiteral == (smt_bvslt(x, y)))

def f_sle_bits(st: LiftState, t: BigInt, x: BitVecLiteral, y: BitVecLiteral): Boolean =
  (TrueLiteral == (smt_bvsle(x, y)))

def f_zeros_bits(st: LiftState, w: BigInt): BitVecLiteral = BitVecLiteral(0, w.toInt)

def f_ones_bits(st: LiftState, w: BigInt): BitVecLiteral = BitVecLiteral(BigInt(2).pow(w.toInt) - 1, w.toInt)

def f_ZeroExtend(st: LiftState, t0: BigInt, t1: BigInt, n: BitVecLiteral, x: BigInt): BitVecLiteral =
  smt_zero_extend(x.toInt - n.size, n)

def f_SignExtend(st: LiftState, t0: BigInt, t1: BigInt, n: BitVecLiteral, x: BigInt): BitVecLiteral =
  smt_sign_extend(x.toInt - n.size, n)

def f_asr_bits(st: LiftState, targ0: BigInt, targ1: BigInt, arg0: BitVecLiteral, arg1: BitVecLiteral): BitVecLiteral =
  smt_bvashr(arg0, arg1)

def f_lsl_bits(st: LiftState, targ0: BigInt, targ1: BigInt, arg0: BitVecLiteral, arg1: BitVecLiteral): BitVecLiteral =
  smt_bvshl(arg0, zero_extend_to(arg0.size, arg1))

def f_lsr_bits(st: LiftState, targ0: BigInt, targ1: BigInt, arg0: BitVecLiteral, arg1: BitVecLiteral): BitVecLiteral =
  smt_bvlshr(arg0, zero_extend_to(arg0.size, arg1))

def f_decl_bool(st: LiftState, arg0: String): RTSym = LocalVar(arg0, BoolType)
def f_decl_bv(st: LiftState, arg0: String, arg1: BigInt): RTSym = LocalVar(arg0, BitVecType(arg1.toInt))

def f_gen_BFAdd(st: LiftState, arg0: RTSym, arg1: RTSym): RTSym = throw NotImplementedError("func not implemented")
def f_gen_BFMul(st: LiftState, arg0: RTSym, arg1: RTSym): RTSym = throw NotImplementedError("func not implemented")

def f_gen_FPAdd(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = throw NotImplementedError(
  "func not implemented"
)
def f_gen_FPCompare(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym, arg3: RTSym): RTSym =
  throw NotImplementedError("func not implemented")
def f_gen_FPCompareEQ(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym =
  throw NotImplementedError("func not implemented")
def f_gen_FPCompareGE(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym =
  throw NotImplementedError("func not implemented")
def f_gen_FPCompareGT(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym =
  throw NotImplementedError("func not implemented")
def f_gen_FPConvert(st: LiftState, targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym =
  throw NotImplementedError("func not implemented")
def f_gen_FPConvertBF(st: LiftState, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = throw NotImplementedError(
  "func not implemented"
)
def f_gen_FPDiv(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = throw NotImplementedError(
  "func not implemented"
)
def f_gen_FPMax(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = throw NotImplementedError(
  "func not implemented"
)
def f_gen_FPMaxNum(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym =
  throw NotImplementedError("func not implemented")
def f_gen_FPMin(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = throw NotImplementedError(
  "func not implemented"
)
def f_gen_FPMinNum(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym =
  throw NotImplementedError("func not implemented")
def f_gen_FPMul(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = throw NotImplementedError(
  "func not implemented"
)
def f_gen_FPMulAdd(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym, arg3: RTSym): RTSym =
  throw NotImplementedError("func not implemented")
def f_gen_FPMulAddH(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym, arg3: RTSym): RTSym =
  throw NotImplementedError("func not implemented")
def f_gen_FPMulX(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym =
  throw NotImplementedError("func not implemented")
def f_gen_FPRSqrtStepFused(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = throw NotImplementedError(
  "func not implemented"
)
def f_gen_FPRecipEstimate(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = throw NotImplementedError(
  "func not implemented"
)
def f_gen_FPRecipStepFused(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = throw NotImplementedError(
  "func not implemented"
)
def f_gen_FPRecpX(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = throw NotImplementedError(
  "func not implemented"
)
def f_gen_FPRoundInt(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym, arg3: RTSym): RTSym =
  throw NotImplementedError("func not implemented")
def f_gen_FPRoundIntN(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym, arg3: RTSym): RTSym =
  throw NotImplementedError("func not implemented")
def f_gen_FPSqrt(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = throw NotImplementedError(
  "func not implemented"
)
def f_gen_FPSub(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = throw NotImplementedError(
  "func not implemented"
)
def f_gen_FPToFixed(
    st: LiftState,
    targ0: BigInt,
    targ1: BigInt,
    arg0: RTSym,
    arg1: RTSym,
    arg2: RTSym,
    arg3: RTSym,
    arg4: RTSym
): RTSym = throw NotImplementedError("func not implemented")
def f_gen_FPToFixedJS_impl(st: LiftState, targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym =
  throw NotImplementedError("func not implemented")
def f_gen_FixedToFP(
    st: LiftState,
    targ0: BigInt,
    targ1: BigInt,
    arg0: RTSym,
    arg1: RTSym,
    arg2: RTSym,
    arg3: RTSym,
    arg4: RTSym
): RTSym = throw NotImplementedError("func not implemented")

def f_gen_bit_lit(st: LiftState, targ0: BigInt, arg0: BitVecLiteral): RTSym = BitVecLiteral(arg0.value, targ0.toInt)
def f_gen_bool_lit(st: LiftState, arg0: Boolean): RTSym = if arg0 then TrueLiteral else FalseLiteral

def f_gen_branch(st: LiftState, arg0: RTSym): RTLabel = st.gen_branch(arg0)
def f_true_branch(st: LiftState, arg0: RTLabel): RTLabel = (st.branches(arg0))._1
def f_false_branch(st: LiftState, arg0: RTLabel): RTLabel = (st.branches(arg0))._2
def f_merge_branch(st: LiftState, arg0: RTLabel): RTLabel = (st.branches(arg0))._3

def f_cvt_bits_uint(st: LiftState, targ0: BigInt, arg0: BitVecLiteral): BigInt = arg0.value
def f_gen_cvt_bits_uint(st: LiftState, targ0: BigInt, arg0: RTSym): RTSym = arg0
def f_gen_cvt_bool_bv(st: LiftState, arg0: RTSym): RTSym = arg0

def f_gen_eor_bits(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = BinaryExpr(BVEQ, arg0, arg1)
def f_gen_eq_bits(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = BinaryExpr(BVEQ, arg0, arg1)
def f_gen_eq_enum(st: LiftState, arg0: RTSym, arg1: RTSym): RTSym = BinaryExpr(BVEQ, arg0, arg1)
def f_gen_int_lit(st: LiftState, arg0: BigInt): BitVecLiteral = BitVecLiteral(arg0, 1123)

def f_gen_store(st: LiftState, lval: RTSym, e: RTSym): Unit = lval match
  case v: Variable => st.push_stmt(LocalAssign(v, e))
  case m           => throw NotImplementedError(s"fail assign $m")

def f_gen_load(st: LiftState, e: RTSym): RTSym = e match
  case m: Memory => throw NotImplementedError()
  case _         => e

def f_gen_SignExtend(st: LiftState, targ0: BigInt, targ1: BigInt, arg0: Expr, arg1: BitVecLiteral): RTSym = {
  val oldSize = (targ0)
  val newSize = (targ1)
  if (arg1.value != newSize) {
    throw Exception()
  }
  SignExtend((newSize - oldSize).toInt, arg0)
}

def f_gen_ZeroExtend(st: LiftState, targ0: BigInt, targ1: BigInt, arg0: Expr, arg1: BitVecLiteral): RTSym = {
  val oldSize = (targ0)
  val newSize = (targ1)
  if (arg1.value != newSize) {
    throw Exception()
  }
  ZeroExtend((newSize - oldSize).toInt, arg0)
}

def f_gen_add_bits(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = BinaryExpr(BVADD, arg0, arg1)
def f_gen_and_bits(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = BinaryExpr(BVAND, arg0, arg1)
def f_gen_and_bool(st: LiftState, arg0: RTSym, arg1: RTSym): RTSym = BinaryExpr(BoolAND, arg0, arg1)

def f_gen_asr_bits(st: LiftState, targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: RTSym): RTSym =
  BinaryExpr(BVASHR, arg0, gen_zero_extend_to(targ0, arg1))
def f_gen_lsl_bits(st: LiftState, targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: RTSym): RTSym =
  BinaryExpr(BVSHL, arg0, gen_zero_extend_to(targ0, arg1))
def f_gen_lsr_bits(st: LiftState, targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: RTSym): RTSym =
  BinaryExpr(BVLSHR, arg0, gen_zero_extend_to(targ0, arg1))
def f_gen_mul_bits(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = BinaryExpr(BVMUL, arg0, arg1)
def f_gen_ne_bits(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = BinaryExpr(BVCOMP, arg0, arg1)
def f_gen_not_bits(st: LiftState, targ0: BigInt, arg0: RTSym): RTSym = arg0.getType match {
  case BoolType       => UnaryExpr(BoolNOT, arg0)
  case BitVecType(_) => UnaryExpr(BVNOT, arg0)
  case _: MapType     => throw IllegalArgumentException()
  case IntType        => throw IllegalArgumentException()
}

def f_gen_not_bool(st: LiftState, arg0: RTSym): RTSym = arg0.getType match {
  case BoolType       => UnaryExpr(BoolNOT, arg0)
  case BitVecType(sz) => BinaryExpr(BVNEQ, BitVecLiteral(0, sz), arg0)
  case _: MapType     => throw IllegalArgumentException()
  case IntType        => throw IllegalArgumentException()
}

def f_gen_or_bits(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = BinaryExpr(BVOR, arg0, arg1)
def f_gen_or_bool(st: LiftState, arg0: RTSym, arg1: RTSym): RTSym = BinaryExpr(BoolOR, arg0, arg1)
def f_gen_sdiv_bits(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = BinaryExpr(BVSDIV, arg0, arg1)
def f_gen_sle_bits(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = BinaryExpr(BVSLE, arg0, arg1)
def f_gen_slt_bits(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = BinaryExpr(BVSLT, arg0, arg1)
def f_gen_sub_bits(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = BinaryExpr(BVSUB, arg0, arg1)

def f_AtomicEnd(st: LiftState): RTSym = Register("ATOMICSTART", BoolType)
def f_AtomicStart(st: LiftState): RTSym = Register("ATOMICSTART", BoolType)

def f_replicate_bits(st: LiftState, targ0: BigInt, targ1: BigInt, arg0: BitVecLiteral, arg1: BigInt): BitVecLiteral = {
  bv_replicate(arg0, arg1.toInt)
}
def f_append_bits(st: LiftState, targ0: BigInt, targ1: BigInt, a: BitVecLiteral, b: BitVecLiteral): BitVecLiteral =
  BitVecLiteral((a.value << b.size) + b.value, (a.size + b.size))

def f_gen_AArch64_MemTag_set(st: LiftState, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = throw NotImplementedError(
  "func not implemented"
)

def f_gen_Mem_set(st: LiftState, sz: BigInt, ptr: RTSym, width: BitVecLiteral, acctype: RTSym, value: RTSym): Unit =
  assert(width.value == sz)
  val expr = MemoryStore(st.memory, ptr, value, st.endian, sz.toInt)
  val stmt = MemoryAssign(st.memory, expr)
  st.push_stmt(stmt)

def f_gen_Mem_read(st: LiftState, targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym =
  val s: Int = arg2 match
    case BitVecLiteral(v, s) => v.toInt
    case IntLiteral(v)       => v.toInt
    case _                   => throw NotImplementedError(s"Cannot convert $arg2 to int")
  MemoryLoad(st.memory, arg1, st.endian, s)

def f_gen_slice(st: LiftState, e: RTSym, lo: BigInt, wd: BigInt): RTSym = {
  Extract((wd + lo).toInt, lo.toInt, e)
}
def f_gen_replicate_bits(st: LiftState, targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: BitVecLiteral): RTSym = {
  Range.Exclusive(1, arg1.value.toInt, 1).map(v => arg0).foldLeft(arg0)((a, b) => (BinaryExpr(BVCONCAT, a, b)))
}
def f_gen_append_bits(st: LiftState, targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: RTSym): RTSym =
  BinaryExpr(BVCONCAT, arg0, arg1)
def f_gen_array_load(st: LiftState, arg0: RTSym, arg1: BigInt): RTSym = arg0 match
  case Register("_R", t) => Register("R" + arg1, BitVecType(64))
  case _ => {
    Logger.warn(s"Unknown array load $arg0")
    arg0
  }
def f_gen_array_store(st: LiftState, arg0: RTSym, arg1: BigInt, arg2: RTSym): Unit = arg0 match
  case Register(n, t) if n.contains("R") => st.push_stmt(LocalAssign(Register("R" + arg1, BitVecType(64)), arg2))
  case _                                 => Logger.warn(s"Unknown array store $arg0")

def f_gen_assert(st: LiftState, arg0: RTSym) = st.push_stmt(Assert(arg0))
def f_switch_context(st: LiftState, arg0: RTLabel) = st.switch_ctx(arg0)

/** Global variable definitions * */

def v_PSTATE_C = Mutable(Register("PSTATE.C", BoolType)) // Expr_Field(Expr_Var(Ident "PSTATE"), Ident "C")
def v_PSTATE_Z = Mutable(Register("PSTATE.Z", BoolType)) // Expr_Field(Expr_Var(Ident "PSTATE"), Ident "Z")
def v_PSTATE_V = Mutable(Register("PSTATE.V", BoolType)) // Expr_Field(Expr_Var(Ident "PSTATE"), Ident "V")
def v_PSTATE_N = Mutable(Register("PSTATE.N", BoolType)) // Expr_Field(Expr_Var(Ident "PSTATE"), Ident "N")
def v__PC = Mutable(Register("_PC", BitVecType(64))) // Expr_Var(Ident "_PC")
def v__R = Mutable(Register("_R", IntType))
def v__Z = Mutable(Register("_Z", BoolType))
def v_SP_EL0 = Mutable(Register("R31", BitVecType(64)))
def v_FPSR = Mutable(Register("FPSR", BoolType))
def v_FPCR = Mutable(LocalVar("FPCR", BitVecType(32)))

def v_PSTATE_BTYPE = Mutable(Register("PSTATE.BTYPE", BoolType)) // Expr_Field(Expr_Var(Ident "PSTATE"), Ident "BTYPE")
def v_BTypeCompatible = Mutable(Register("BTypeCompatible", BoolType)) // Expr_Var (Ident "BTypeCompatible")
def v___BranchTaken = Mutable(Register("__BranchTaken", BoolType))
def v_BTypeNext = Mutable(Register("BTypeNext", BoolType))
def v___ExclusiveLocal = Mutable(Register("__ExclusiveLocal", BoolType))
