#pragma once

#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/MC/MCExpr.h>

namespace aslp {

template <typename BITS, typename BIGINT, typename RT_EXPR, typename RT_LEXPR,
          typename RT_LABEL>
class lifter_interface {
public:
  // bits which are known at lift-time
  using bits = BITS;
  // bigints which are known at lift-time
  using bigint = BIGINT;

  // runtime-expression type, i.e. the type of values produced by the semantics
  using rt_expr = RT_EXPR;
  using rt_lexpr = RT_EXPR;
  // runtime-label, supports switching blocks during semantics generation
  using rt_label = RT_LABEL;

protected:
  virtual bits extract_bits(const bits &val, bigint lo, bigint wd) = 0;

  virtual bool f_eq_bits(const bits &x, const bits &y) = 0;
  virtual bool f_ne_bits(const bits &x, const bits &y) = 0;
  virtual bits f_add_bits(const bits &x, const bits &y) = 0;
  virtual bits f_sub_bits(const bits &x, const bits &y) = 0;
  virtual bits f_mul_bits(const bits &x, const bits &y) = 0;
  virtual bits f_and_bits(const bits &x, const bits &y) = 0;
  virtual bits f_or_bits(const bits &x, const bits &y) = 0;
  virtual bits f_eor_bits(const bits &x, const bits &y) = 0;
  virtual bits f_not_bits(const bits &x) = 0;
  virtual bool f_slt_bits(const bits &x, const bits &y) = 0;
  virtual bool f_sle_bits(const bits &x, const bits &y) = 0;
  virtual bits f_zeros_bits(bigint n) = 0;
  virtual bits f_ones_bits(bigint n) = 0;
  virtual bits f_replicate_bits(const bits &x, bigint n) = 0;
  virtual bits f_append_bits(const bits &x, const bits &y) = 0;
  virtual bits f_ZeroExtend(const bits &x, bigint wd) = 0;
  virtual bits f_SignExtend(const bits &x, bigint wd) = 0;
  virtual bits f_lsl_bits(const bits &x, const bits &y) = 0;
  virtual bits f_lsr_bits(const bits &x, const bits &y) = 0;
  virtual bits f_asr_bits(const bits &x, const bits &y) = 0;
  virtual bigint f_cvt_bits_uint(const bits &x) = 0;

  virtual rt_expr f_decl_bv(std::string_view name, bigint width) = 0;
  virtual rt_expr f_decl_bool(std::string_view name) = 0;

  virtual void f_switch_context(rt_label label) = 0;

  virtual std::tuple<rt_label, rt_label, rt_label>
  f_gen_branch(rt_expr cond) = 0;

  virtual void f_gen_assert(rt_expr cond) = 0;
  virtual rt_expr f_gen_bit_lit(bits bits) = 0;
  virtual rt_expr f_gen_bool_lit(bool b) = 0;
  virtual rt_expr f_gen_int_lit(bigint i) = 0;
  virtual rt_expr f_gen_load(rt_lexpr ptr) = 0;
  virtual void f_gen_store(rt_lexpr var, rt_expr exp) = 0;
  virtual void
  f_gen_array_load(rt_lexpr array,
                   rt_expr index) = 0; // XXX unsure of array ptr type.
  virtual void f_gen_array_store(rt_lexpr array, rt_expr index, rt_expr exp) = 0;
  virtual void f_gen_Mem_set(rt_expr ptr, rt_expr width, rt_expr acctype,
                             rt_expr exp) = 0;
  virtual rt_expr f_gen_Mem_read(rt_expr ptr, rt_expr width,
                                 rt_expr acctype) = 0;
  virtual void f_gen_AArch64_MemTag_set(rt_expr desc, rt_expr value) = 0;

  virtual rt_expr f_gen_cvt_bits_uint(rt_expr bits) = 0;
  virtual rt_expr f_gen_cvt_bool_bv(rt_expr e) = 0;

  virtual rt_expr f_gen_not_bool(rt_expr e) = 0;
  virtual rt_expr f_gen_and_bool(rt_expr x, rt_expr y) = 0;
  virtual rt_expr f_gen_or_bool(rt_expr x, rt_expr y) = 0;
  virtual rt_expr f_gen_eq_enum(rt_expr x, rt_expr y) = 0;
  virtual rt_expr f_gen_eq_bits(rt_expr x, rt_expr y) = 0;
  virtual rt_expr f_gen_ne_bits(rt_expr x, rt_expr y) = 0;
  virtual rt_expr f_gen_not_bits(rt_expr x, rt_expr y) = 0;
  virtual rt_expr f_gen_or_bits(rt_expr x, rt_expr y) = 0;
  virtual rt_expr f_gen_eor_bits(rt_expr x, rt_expr y) = 0;
  virtual rt_expr f_gen_and_bits(rt_expr x, rt_expr y) = 0;

  virtual rt_expr f_gen_add_bits(rt_expr x, rt_expr y) = 0;
  virtual rt_expr f_gen_sub_bits(rt_expr x, rt_expr y) = 0;
  virtual rt_expr f_gen_sdiv_bits(rt_expr x, rt_expr y) = 0;
  virtual rt_expr f_gen_sle_bits(rt_expr x, rt_expr y) = 0;
  virtual rt_expr f_gen_slt_bits(rt_expr x, rt_expr y) = 0;
  virtual rt_expr f_gen_mul_bits(rt_expr x, rt_expr y) = 0;

  virtual rt_expr f_gen_append_bits(rt_expr x, rt_expr y) = 0;
  virtual rt_expr f_gen_lsr_bits(rt_expr x, rt_expr y) = 0;
  virtual rt_expr f_gen_lsl_bits(rt_expr x, rt_expr y) = 0;
  virtual rt_expr f_gen_asr_bits(rt_expr x, rt_expr y) = 0;
  virtual rt_expr f_gen_replicate_bits(rt_expr x, rt_expr y) = 0;
  virtual rt_expr f_gen_ZeroExtend(rt_expr x, rt_expr y) = 0;
  virtual rt_expr f_gen_SignExtend(rt_expr x, rt_expr y) = 0;
  virtual rt_expr f_gen_slice(rt_expr e, rt_expr lo, rt_expr wd) = 0;

  virtual rt_expr f_gen_FPCompare(rt_expr x, rt_expr y, rt_expr signalnan,
                                  rt_expr fpcr) = 0;
  virtual rt_expr f_gen_FPCompareEQ(rt_expr x, rt_expr y, rt_expr fpcr) = 0;
  virtual rt_expr f_gen_FPCompareGE(rt_expr x, rt_expr y, rt_expr fpcr) = 0;
  virtual rt_expr f_gen_FPCompareGT(rt_expr x, rt_expr y, rt_expr fpcr) = 0;
  virtual rt_expr f_gen_FPAdd(rt_expr x, rt_expr y, rt_expr fpcr) = 0;
  virtual rt_expr f_gen_FPSub(rt_expr x, rt_expr y, rt_expr fpcr) = 0;
  virtual rt_expr f_gen_FPMulAdda(rt_expr x, rt_expr y, rt_expr fpcr) = 0;
  virtual rt_expr f_gen_FPMulAddHa(rt_expr x, rt_expr y, rt_expr fpcr) = 0;
  virtual rt_expr f_gen_FPMulX(rt_expr x, rt_expr y, rt_expr fpcr) = 0;
  virtual rt_expr f_gen_FPMul(rt_expr x, rt_expr y, rt_expr fpcr) = 0;
  virtual rt_expr f_gen_FPDiv(rt_expr x, rt_expr y, rt_expr fpcr) = 0;
  virtual rt_expr f_gen_FPMin(rt_expr x, rt_expr y, rt_expr fpcr) = 0;
  virtual rt_expr f_gen_FPMinNum(rt_expr x, rt_expr y, rt_expr fpcr) = 0;
  virtual rt_expr f_gen_FPMax(rt_expr x, rt_expr y, rt_expr fpcr) = 0;
  virtual rt_expr f_gen_FPMaxNum(rt_expr x, rt_expr y, rt_expr fpcr) = 0;
  virtual rt_expr f_gen_FPRecpX(rt_expr x, rt_expr fpcr) = 0;
  virtual rt_expr f_gen_FPSqrt(rt_expr x, rt_expr fpcr) = 0;
  virtual rt_expr f_gen_FPRecipEstimate(rt_expr x, rt_expr fpcr) = 0;
  virtual rt_expr f_gen_BFAdd(rt_expr x, rt_expr y) = 0;
  virtual rt_expr f_gen_BFMul(rt_expr x, rt_expr y) = 0;
  virtual rt_expr f_gen_FPConvertBF(rt_expr x, rt_expr fpcr,
                                    rt_expr rounding) = 0;
  virtual rt_expr f_gen_FPRecipStepFused(rt_expr x, rt_expr y) = 0;
  virtual rt_expr f_gen_FPRSqrtStepFused(rt_expr x, rt_expr y) = 0;
  virtual rt_expr f_gen_FPToFixed(rt_expr x, rt_expr fbits, rt_expr isunsigned,
                                  rt_expr fpcr, rt_expr rounding) = 0;
  virtual rt_expr f_gen_FixedToFP(rt_expr x, rt_expr fbits, rt_expr isunsigned,
                                  rt_expr fpcr, rt_expr rounding) = 0;
  virtual rt_expr f_gen_FPConvert(rt_expr x, rt_expr fpcr,
                                  rt_expr rounding) = 0;
  virtual rt_expr f_gen_FPRoundInt(rt_expr x, rt_expr fpcr, rt_expr rounding,
                                   rt_expr isexact) = 0;
  virtual rt_expr f_gen_FPRoundIntN(rt_expr x, rt_expr fpcr, rt_expr rounding,
                                    rt_expr intsize) = 0;
  virtual rt_expr f_gen_FPToFixedJS_impl(rt_expr x, rt_expr fpcr,
                                         rt_expr is64) = 0; // from override.asl
};

namespace llvm_detail {
using bits = llvm::APInt;
using bigint = long long;
using rt_expr = llvm::Value *;
using rt_lexpr = llvm::AllocaInst *;
using rt_label = llvm::BasicBlock *;

using lifter_interface_llvm =
    lifter_interface<bits, bigint, rt_expr, rt_lexpr, rt_label>;
} // namespace llvm_detail

using llvm_detail::lifter_interface_llvm;

} // namespace aslp
