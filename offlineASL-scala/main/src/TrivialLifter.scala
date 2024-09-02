package main
import lifter._

import scala.collection.mutable.{LinearSeq, Seq, Buffer}

/**
 * User-provided implementation of the lift-time semantics (e.g. f_eq_bits) and IBI (e.g. f_gen_eq_bits).
 */

class TrivialLifter extends LiftState[Any, Any, Any] {
  type BV = Any /* Lift-time bitvector representation, must be BV <: RTSym */
  type RTSym = Any /* Run-time expression representation */
  type RTLabel = Any  /* Block/branch labels for gen_branch */

  private var current: Buffer[Any] = Buffer()
  private def emit(x: RTSym) = current += x

  /* Lift-time semantics */
  override def mkBits(n: BigInt, y: BigInt): BV = Buffer("mkBits", n, y)
  override def bvextract(e: BV, lo: BigInt, width: BigInt): BV = Buffer("bvextract", e, lo, width)
  override def f_eq_bits(t: BigInt, x: BV, y: BV): Boolean = x == y
  override def f_ne_bits(t: BigInt, x: BV, y: BV): Boolean = x != y
  override def f_add_bits(t: BigInt, x: BV, y: BV): BV = Buffer("f_add_bits", t, x, y)
  override def f_sub_bits(t: BigInt, x: BV, y: BV): BV = Buffer("f_sub_bits", t, x, y)
  override def f_mul_bits(t: BigInt, x: BV, y: BV): BV = Buffer("f_mul_bits", t, x, y)
  override def f_and_bits(t: BigInt, x: BV, y: BV): BV = Buffer("f_and_bits", t, x, y)
  override def f_or_bits(t: BigInt, x: BV, y: BV): BV = Buffer("f_or_bits", t, x, y)
  override def f_eor_bits(t: BigInt, x: BV, y: BV): BV = Buffer("f_eor_bits", t, x, y)
  override def f_not_bits(t: BigInt, x: BV): BV = Buffer("f_not_bits", t, x)
  override def f_slt_bits(t: BigInt, x: BV, y: BV): Boolean = False
  override def f_sle_bits(t: BigInt, x: BV, y: BV): Boolean = False
  override def f_zeros_bits(w: BigInt): BV = Buffer("f_zeros_bits", w)
  override def f_ones_bits(w: BigInt): BV = Buffer("f_ones_bits", w)
  override def f_ZeroExtend(t0: BigInt, t1: BigInt, n: BV, x: BigInt): BV = Buffer("f_ZeroExtend", t0, t1, n, x)
  override def f_SignExtend(t0: BigInt, t1: BigInt, n: BV, x: BigInt): BV = Buffer("f_SignExtend", t0, t1, n, x)
  override def f_asr_bits(targ0: BigInt, targ1: BigInt, arg0: BV, arg1: BV): BV = Buffer("f_asr_bits", targ0, targ1, arg0, arg1)
  override def f_lsl_bits(targ0: BigInt, targ1: BigInt, arg0: BV, arg1: BV): BV = Buffer("f_lsl_bits", targ0, targ1, arg0, arg1)
  override def f_lsr_bits(targ0: BigInt, targ1: BigInt, arg0: BV, arg1: BV): BV = Buffer("f_lsr_bits", targ0, targ1, arg0, arg1)
  override def f_decl_bool(arg0: String): RTSym = emit(Buffer("f_decl_bool", arg0))
  override def f_decl_bv(arg0: String, arg1: BigInt): RTSym = emit(Buffer("f_decl_bv", arg0, arg1))
  override def f_AtomicEnd(): RTSym = emit(Buffer("f_AtomicEnd"))
  override def f_AtomicStart(): RTSym = emit(Buffer("f_AtomicStart"))

  override def f_replicate_bits(targ0: BigInt, targ1: BigInt, arg0: BV, arg1: BigInt): BV = Buffer("f_replicate_bits", targ0, targ1, arg0, arg1)
  override def f_append_bits(targ0: BigInt, targ1: BigInt, a: BV, b: BV): BV = Buffer("f_append_bits", targ0, targ1, a, b)

  /** Run-time IR program generation */

  override def f_gen_BFAdd(arg0: RTSym, arg1: RTSym): RTSym = emit(Buffer("f_gen_BFAdd", arg0, arg1))
  override def f_gen_BFMul(arg0: RTSym, arg1: RTSym): RTSym = emit(Buffer("f_gen_BFMul", arg0, arg1))
  override def f_gen_FPAdd(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = emit(Buffer("f_gen_FPAdd", targ0, arg0, arg1, arg2))
  override def f_gen_FPCompare(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym, arg3: RTSym): RTSym = emit(Buffer("f_gen_FPCompare", targ0, arg0, arg1, arg2, arg3))
  override def f_gen_FPCompareEQ(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = emit(Buffer("f_gen_FPCompareEQ", targ0, arg0, arg1, arg2))
  override def f_gen_FPCompareGE(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = emit(Buffer("f_gen_FPCompareGE", targ0, arg0, arg1, arg2))
  override def f_gen_FPCompareGT(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = emit(Buffer("f_gen_FPCompareGT", targ0, arg0, arg1, arg2))
  override def f_gen_FPConvert(targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = emit(Buffer("f_gen_FPConvert", targ0, targ1, arg0, arg1, arg2))
  override def f_gen_FPConvertBF(arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = emit(Buffer("f_gen_FPConvertBF", arg0, arg1, arg2))
  override def f_gen_FPDiv(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = emit(Buffer("f_gen_FPDiv", targ0, arg0, arg1, arg2))
  override def f_gen_FPMax(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = emit(Buffer("f_gen_FPMax", targ0, arg0, arg1, arg2))
  override def f_gen_FPMaxNum(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = emit(Buffer("f_gen_FPMaxNum", targ0, arg0, arg1, arg2))
  override def f_gen_FPMin(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = emit(Buffer("f_gen_FPMin", targ0, arg0, arg1, arg2))
  override def f_gen_FPMinNum(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = emit(Buffer("f_gen_FPMinNum", targ0, arg0, arg1, arg2))
  override def f_gen_FPMul(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = emit(Buffer("f_gen_FPMul", targ0, arg0, arg1, arg2))
  override def f_gen_FPMulAdd(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym, arg3: RTSym): RTSym = emit(Buffer("f_gen_FPMulAdd", targ0, arg0, arg1, arg2, arg3))
  override def f_gen_FPMulAddH(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym, arg3: RTSym): RTSym = emit(Buffer("f_gen_FPMulAddH", targ0, arg0, arg1, arg2, arg3))
  override def f_gen_FPMulX(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = emit(Buffer("f_gen_FPMulX", targ0, arg0, arg1, arg2))
  override def f_gen_FPRSqrtStepFused(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = emit(Buffer("f_gen_FPRSqrtStepFused", targ0, arg0, arg1))
  override def f_gen_FPRecipEstimate(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = emit(Buffer("f_gen_FPRecipEstimate", targ0, arg0, arg1))
  override def f_gen_FPRecipStepFused(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = emit(Buffer("f_gen_FPRecipStepFused", targ0, arg0, arg1))
  override def f_gen_FPRecpX(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = emit(Buffer("f_gen_FPRecpX", targ0, arg0, arg1))
  override def f_gen_FPRoundInt(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym, arg3: RTSym): RTSym = emit(Buffer("f_gen_FPRoundInt", targ0, arg0, arg1, arg2, arg3))
  override def f_gen_FPRoundIntN(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym, arg3: RTSym): RTSym = emit(Buffer("f_gen_FPRoundIntN", targ0, arg0, arg1, arg2, arg3))
  override def f_gen_FPSqrt(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = emit(Buffer("f_gen_FPSqrt", targ0, arg0, arg1))
  override def f_gen_FPSub(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = emit(Buffer("f_gen_FPSub", targ0, arg0, arg1, arg2))
  override def f_gen_FPToFixed(targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym, arg3: RTSym, arg4: RTSym): RTSym = emit(Buffer("f_gen_FPToFixed", targ0, targ1, arg0, arg1, arg2, arg3, arg4))
  override def f_gen_FPToFixedJS_impl(targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = emit(Buffer("f_gen_FPToFixedJS_impl", targ0, targ1, arg0, arg1, arg2))
  override def f_gen_FixedToFP(targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym, arg3: RTSym, arg4: RTSym): RTSym = emit(Buffer("f_gen_FixedToFP", targ0, targ1, arg0, arg1, arg2, arg3, arg4))
  override def f_gen_bit_lit(targ0: BigInt, arg0: BV): RTSym = emit(Buffer("f_gen_bit_lit", targ0, arg0))
  override def f_gen_bool_lit(arg0: Boolean): RTSym = emit(Buffer("f_gen_bool_lit", arg0))
  override def f_gen_branch(arg0: RTSym): RTLabel = Buffer("f_gen_branch", arg0)
  override def f_cvt_bits_uint(targ0: BigInt, arg0: BV): BigInt = Buffer("f_cvt_bits_uint", targ0, arg0)
  override def f_gen_cvt_bits_uint(targ0: BigInt, arg0: RTSym): RTSym = emit(Buffer("f_gen_cvt_bits_uint", targ0, arg0))
  override def f_gen_cvt_bool_bv(arg0: RTSym): RTSym = emit(Buffer("f_gen_cvt_bool_bv", arg0))
  override def f_gen_eor_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = emit(Buffer("f_gen_eor_bits", targ0, arg0, arg1))
  override def f_gen_eq_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = emit(Buffer("f_gen_eq_bits", targ0, arg0, arg1))
  override def f_gen_eq_enum(arg0: RTSym, arg1: RTSym): RTSym = emit(Buffer("f_gen_eq_enum", arg0, arg1))
  override def f_gen_int_lit(arg0: BigInt): BV = Buffer("f_gen_int_lit", arg0)
  override def f_gen_store(lval: RTSym, e: RTSym): Unit = Buffer("f_gen_store", lval, e)
  override def f_gen_load(e: RTSym): RTSym = emit(Buffer("f_gen_load", e))
  override def f_gen_SignExtend(targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: BV): RTSym = emit(Buffer("f_gen_SignExtend", targ0, targ1, arg0, arg1))
  override def f_gen_ZeroExtend(targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: BV): RTSym = emit(Buffer("f_gen_ZeroExtend", targ0, targ1, arg0, arg1))
  override def f_gen_add_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = emit(Buffer("f_gen_add_bits", targ0, arg0, arg1))
  override def f_gen_and_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = emit(Buffer("f_gen_and_bits", targ0, arg0, arg1))
  override def f_gen_and_bool(arg0: RTSym, arg1: RTSym): RTSym = emit(Buffer("f_gen_and_bool", arg0, arg1))
  override def f_gen_asr_bits(targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: RTSym): RTSym = emit(Buffer("f_gen_asr_bits", targ0, targ1, arg0, arg1))
  override def f_gen_lsl_bits(targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: RTSym): RTSym = emit(Buffer("f_gen_lsl_bits", targ0, targ1, arg0, arg1))
  override def f_gen_lsr_bits(targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: RTSym): RTSym = emit(Buffer("f_gen_lsr_bits", targ0, targ1, arg0, arg1))
  override def f_gen_mul_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = emit(Buffer("f_gen_mul_bits", targ0, arg0, arg1))
  override def f_gen_ne_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = emit(Buffer("f_gen_ne_bits", targ0, arg0, arg1))
  override def f_gen_not_bits(targ0: BigInt, arg0: RTSym): RTSym = emit(Buffer("f_gen_not_bits", targ0, arg0))
  override def f_gen_not_bool(arg0: RTSym): RTSym = emit(Buffer("f_gen_not_bool", arg0))
  override def f_gen_or_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = emit(Buffer("f_gen_or_bits", targ0, arg0, arg1))
  override def f_gen_or_bool(arg0: RTSym, arg1: RTSym): RTSym = emit(Buffer("f_gen_or_bool", arg0, arg1))
  override def f_gen_sdiv_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = emit(Buffer("f_gen_sdiv_bits", targ0, arg0, arg1))
  override def f_gen_sle_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = emit(Buffer("f_gen_sle_bits", targ0, arg0, arg1))
  override def f_gen_slt_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = emit(Buffer("f_gen_slt_bits", targ0, arg0, arg1))
  override def f_gen_sub_bits(targ0: BigInt, arg0: RTSym, arg1: RTSym): RTSym = emit(Buffer("f_gen_sub_bits", targ0, arg0, arg1))
  override def f_gen_AArch64_MemTag_set(arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = emit(Buffer("f_gen_AArch64_MemTag_set", arg0, arg1, arg2))
  override def f_gen_Mem_read(targ0: BigInt, arg0: RTSym, arg1: RTSym, arg2: RTSym): RTSym = emit(Buffer("f_gen_Mem_read", targ0, arg0, arg1, arg2))
  override def f_gen_slice(e: RTSym, lo: BigInt, wd: BigInt): RTSym = emit(Buffer("f_gen_slice", e, lo, wd))
  override def f_gen_replicate_bits(targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: BV): RTSym = emit(Buffer("f_gen_replicate_bits", targ0, targ1, arg0, arg1))
  override def f_gen_append_bits(targ0: BigInt, targ1: BigInt, arg0: RTSym, arg1: RTSym): RTSym = emit(Buffer("f_gen_append_bits", targ0, targ1, arg0, arg1))
  override def f_gen_array_load(arg0: RTSym, arg1: BigInt): RTSym = emit(Buffer("f_gen_array_load", arg0, arg1))
  override def f_gen_array_store(arg0: RTSym, arg1: BigInt, arg2: RTSym): Unit = Buffer("f_gen_array_store", arg0, arg1, arg2)
  override def f_gen_Mem_set(sz: BigInt, ptr: RTSym, width: BV, acctype: RTSym, value: RTSym): Unit = Buffer("f_gen_Mem_set", sz, ptr, width, acctype, value)
  override def f_gen_assert(arg0: RTSym): Unit = Buffer("f_gen_assert", arg0)

  override def f_switch_context(arg0: RTLabel): Unit = Buffer("f_switch_context", arg0)
  override def f_true_branch(arg0: RTLabel): RTLabel = Buffer("f_true_branch", arg0)
  override def f_false_branch(arg0: RTLabel): RTLabel = Buffer("f_false_branch", arg0)
  override def f_merge_branch(arg0: RTLabel): RTLabel = Buffer("f_merge_branch", arg0)


  /** Global variable override definitions * */

  override def rTLabelDefault : RTLabel = throw NotImplementedError()
  override def rTSymDefault : RTSym = throw NotImplementedError()
  override def rTExprDefault : RTSym = throw NotImplementedError()

  override def v_PSTATE_UAO: Mutable[RTSym] = throw NotImplementedError()
  override def v_PSTATE_PAN: Mutable[RTSym] = throw NotImplementedError()
  override def v_PSTATE_DIT: Mutable[RTSym] = throw NotImplementedError()
  override def v_PSTATE_SSBS: Mutable[RTSym] = throw NotImplementedError()
  override def v_PSTATE_G: Mutable[RTSym] = throw NotImplementedError()
  override def v_PSTATE_A: Mutable[RTSym] = throw NotImplementedError()
  override def v_PSTATE_I: Mutable[RTSym] = throw NotImplementedError()
  override def v_PSTATE_F: Mutable[RTSym] = throw NotImplementedError()
  override def v_PSTATE_D: Mutable[RTSym] = throw NotImplementedError()
  override def v_PSTATE_C: Mutable[RTSym] = throw NotImplementedError()
  override def v_PSTATE_Z: Mutable[RTSym] = throw NotImplementedError()
  override def v_PSTATE_V: Mutable[RTSym] = throw NotImplementedError()
  override def v_PSTATE_N: Mutable[RTSym] = throw NotImplementedError()
  override def v__PC: Mutable[RTSym] = throw NotImplementedError()
  override def v__R: Mutable[RTSym] = throw NotImplementedError()
  override def v__Z: Mutable[RTSym] = throw NotImplementedError()
  override def v_SP_EL0: Mutable[RTSym] = throw NotImplementedError()
  override def v_FPSR: Mutable[RTSym] = throw NotImplementedError()
  override def v_FPCR: Mutable[RTSym] = throw NotImplementedError()

  override def v_PSTATE_BTYPE: Mutable[RTSym] = throw NotImplementedError()
  override def v_BTypeCompatible: Mutable[RTSym] = throw NotImplementedError()
  override def v___BranchTaken: Mutable[RTSym] = throw NotImplementedError()
  override def v_BTypeNext: Mutable[RTSym] = throw NotImplementedError()
  override def v___ExclusiveLocal: Mutable[RTSym] = throw NotImplementedError()

}


object TrivialLifter {

  def liftOpcode(op: BigInt, sp: BigInt) : Unit = {
    f_A64_decoder[Any, Any, Any](TrivialLifter(), BitVec(op, 32), BitVec(sp, 64))
  }

  def liftOpcode(op: BigInt) : Unit= {
    liftOpcode(op, 0)
  }

}
