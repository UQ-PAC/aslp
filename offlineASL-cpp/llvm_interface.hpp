#include <charconv>
#include <llvm/ADT/APInt.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IRBuilder.h>

#include <stdexcept>
#include <string>
#include <type_traits>
#include <memory>

#include "interface.hpp"

namespace aslp {

struct llvm_lifter_traits {
  using bits = llvm::APInt;
  using bigint = long long;
  using rt_expr = llvm::Value *;
  using rt_lexpr = llvm::AllocaInst *;
  using rt_label = std::shared_ptr<llvm::IRBuilder<>>;
};

static_assert(lifter_traits<llvm_lifter_traits>);

class llvm_lift_time_interface : virtual public lifter_interface<llvm_lifter_traits> {
public:
  bits bits_lit(unsigned width, std::string_view str) override {
   return llvm::APInt{width, str, (char)2};
  }
  bits bits_zero(unsigned width) override {
   return llvm::APInt::getZero(width);
  }
  bigint bigint_lit(std::string_view str) override {
    bigint x;
    auto result = std::from_chars(str.cbegin(), str.cend(), x);

    if (result.ec == std::errc{} && result.ptr == str.cend()) {
     return x;
    } else {
      throw std::invalid_argument{"invalid bigint literal: " + std::string{str}};
    }
  }
  bigint bigint_zero() override {
   return 0LL;
  }

  bits extract_bits(const bits &val, bigint lo, bigint wd) override {
   return val.extractBits(wd, lo);
  }
  bool f_eq_bits(const bits &x, const bits &y) override {
   return x == y;
  }
  bool f_ne_bits(const bits &x, const bits &y) override {
   return x != y;
  }
  bits f_add_bits(const bits &x, const bits &y) override {
   return x + y;
  }
  bits f_sub_bits(const bits &x, const bits &y) override {
   return x - y;
  }
  bits f_mul_bits(const bits &x, const bits &y) override {
   return x * y;
  }
  bits f_and_bits(const bits &x, const bits &y) override {
   return x & y;
  }
  bits f_or_bits(const bits &x, const bits &y) override {
   return x | y;
  }
  bits f_eor_bits(const bits &x, const bits &y) override {
   return x ^ y;
  }
  bits f_not_bits(const bits &x) override {
   return ~x;
  }
  bool f_slt_bits(const bits &x, const bits &y) override {
   return x.slt(y);
  }
  bool f_sle_bits(const bits &x, const bits &y) override {
   return x.sle(y);
  }
  bits f_zeros_bits(bigint n) override {
   return llvm::APInt::getZero(n);
  }
  bits f_ones_bits(bigint n) override {
   return llvm::APInt::getAllOnes(n);
  }
  bits f_replicate_bits(const bits &x, bigint n) override {
    assert(n >= 1);
    bits original = x;
    unsigned wd = x.getBitWidth();
    auto ret = x.zextOrTrunc(wd * n);
    for (unsigned i = 1; i < n; i++) {
     ret <<= wd;
     ret |= original;
    }
   return ret;
  }
  bits f_append_bits(const bits &x, const bits &y) override {
    auto ret = x.zext(x.getBitWidth() + y.getBitWidth());
   ret <<= y.getBitWidth();
   ret |= y;
   return ret;
  }
  bits f_ZeroExtend(const bits &x, bigint wd) override {
   return x.zextOrTrunc(wd);
  }
  bits f_SignExtend(const bits &x, bigint wd) override {
   return x.sextOrTrunc(wd);
  }
  bits f_lsl_bits(const bits &x, const bits &y) override {
   return x << y;
  }
  bits f_lsr_bits(const bits &x, const bits &y) override {
   return x.lshr(y);
  }
  bits f_asr_bits(const bits &x, const bits &y) override {
   return x.ashr(y);
  }
  bigint f_cvt_bits_uint(const bits &x) override {
   return x.getZExtValue();
  }

};


class llvm_run_time_interface : virtual public lifter_interface<llvm_lifter_traits> {
  llvm::LLVMContext &context;
  llvm::Function &function;

  rt_label builder;

protected:
  llvm::Type* intty(unsigned width) {
    return llvm::Type::getIntNTy(context, width);
  }

public:
  llvm_run_time_interface(llvm::Function &f)
    : function{f},
      builder{std::make_shared<llvm::IRBuilder<>>(&f.getEntryBlock())},
      context{f.getContext()} {}

  rt_lexpr v_PSTATE_C() override { assert(0); };
  rt_lexpr v_PSTATE_Z() override { assert(0); };
  rt_lexpr v_PSTATE_V() override { assert(0); };
  rt_lexpr v_PSTATE_N() override { assert(0); };
  rt_lexpr v__PC() override { assert(0); };
  rt_lexpr v__R() override { assert(0); };
  rt_lexpr v__Z() override { assert(0); };
  rt_lexpr v_SP_EL0() override { assert(0); };
  rt_lexpr v_FPSR() override { assert(0); };
  rt_lexpr v_FPCR() override { assert(0); };

  rt_lexpr v_PSTATE_BTYPE() override { assert(0); };
  rt_lexpr v_BTypeCompatible() override { assert(0); };
  rt_lexpr v___BranchTaken() override { assert(0); };
  rt_lexpr v_BTypeNext() override { assert(0); };
  rt_lexpr v___ExclusiveLocal() override { assert(0); };


  rt_lexpr f_decl_bv(std::string_view name, bigint width) override { 
    return builder->CreateAlloca(intty(width), /*addrspace*/0, /*arraysize*/nullptr, name);
  }
  rt_lexpr f_decl_bool(std::string_view name) override {
    return f_decl_bv(name, 1);
  }

  void f_switch_context(rt_label label) override { builder.swap(label); }

  std::tuple<rt_label, rt_label, rt_label> f_gen_branch(rt_expr cond) override {
    auto tcase = llvm::BasicBlock::Create(context, "true", &function);
    auto fcase = llvm::BasicBlock::Create(context, "false", &function);
    auto join = llvm::BasicBlock::Create(context, "join", &function);

    builder->CreateCondBr(cond, tcase, fcase);

    rt_label tlabel = std::make_shared<llvm::IRBuilder<>>(llvm::BranchInst::Create(join, tcase));
    rt_label flabel = std::make_shared<llvm::IRBuilder<>>(llvm::BranchInst::Create(join, fcase));
    rt_label jlabel = std::make_shared<llvm::IRBuilder<>>(join);

    return std::make_tuple(std::move(tlabel), std::move(flabel), std::move(jlabel));
  }

  void f_gen_assert(rt_expr cond) override { assert(0); }
  rt_expr f_gen_bit_lit(bits bits) override { 
    return llvm::ConstantInt::get(intty(bits.getBitWidth()), bits);
  }
  rt_expr f_gen_bool_lit(bool b) override {
    return f_gen_bit_lit(b ? llvm::APInt::getAllOnes(1) : llvm::APInt::getZero(1));
  }
  rt_expr f_gen_int_lit(bigint i) override {
    return llvm::ConstantInt::get(intty(111), i); // XXX hopefully this is never needed
  }

  rt_expr f_gen_load(rt_lexpr ptr) override { assert(0); }
  void f_gen_store(rt_lexpr var, rt_expr exp) override { assert(0); }
  rt_expr f_gen_array_load(rt_lexpr array, bigint index) override { assert(0); }
  void f_gen_array_store(rt_lexpr array, bigint index, rt_expr exp) override { assert(0); }
  void f_gen_Mem_set(rt_expr ptr, rt_expr width, rt_expr acctype, rt_expr exp) override { assert(0); }
  rt_expr f_gen_Mem_read(rt_expr ptr, rt_expr width, rt_expr acctype) override { assert(0); }
  void f_gen_AArch64_MemTag_set(rt_expr address, rt_expr acctype, rt_expr value) override { assert(0); }

  void f_AtomicStart() override { assert(0); }
  void f_AtomicEnd() override { assert(0); }

  rt_expr f_gen_cvt_bits_uint(rt_expr bits) override { assert(0); }
  rt_expr f_gen_cvt_bool_bv(rt_expr e) override { assert(0); }

  rt_expr f_gen_not_bool(rt_expr e) override { assert(0); }
  rt_expr f_gen_and_bool(rt_expr x, rt_expr y) override { assert(0); }
  rt_expr f_gen_or_bool(rt_expr x, rt_expr y) override { assert(0); }
  rt_expr f_gen_eq_enum(rt_expr x, rt_expr y) override { assert(0); }

  rt_expr f_gen_not_bits(rt_expr x) override { assert(0); }
  rt_expr f_gen_eq_bits(rt_expr x, rt_expr y) override { assert(0); }
  rt_expr f_gen_ne_bits(rt_expr x, rt_expr y) override { assert(0); }
  rt_expr f_gen_or_bits(rt_expr x, rt_expr y) override { assert(0); }
  rt_expr f_gen_eor_bits(rt_expr x, rt_expr y) override { assert(0); }
  rt_expr f_gen_and_bits(rt_expr x, rt_expr y) override { assert(0); }

  rt_expr f_gen_add_bits(rt_expr x, rt_expr y) override { assert(0); }
  rt_expr f_gen_sub_bits(rt_expr x, rt_expr y) override { assert(0); }
  rt_expr f_gen_sdiv_bits(rt_expr x, rt_expr y) override { assert(0); }
  rt_expr f_gen_sle_bits(rt_expr x, rt_expr y) override { assert(0); }
  rt_expr f_gen_slt_bits(rt_expr x, rt_expr y) override { assert(0); }
  rt_expr f_gen_mul_bits(rt_expr x, rt_expr y) override { assert(0); }

  rt_expr f_gen_append_bits(rt_expr x, rt_expr y) override { assert(0); }
  rt_expr f_gen_lsr_bits(rt_expr x, rt_expr y) override { assert(0); }
  rt_expr f_gen_lsl_bits(rt_expr x, rt_expr y) override { assert(0); }
  rt_expr f_gen_asr_bits(rt_expr x, rt_expr y) override { assert(0); }
  rt_expr f_gen_replicate_bits(rt_expr x, rt_expr y) override { assert(0); }
  rt_expr f_gen_ZeroExtend(rt_expr x, rt_expr y) override { assert(0); }
  rt_expr f_gen_SignExtend(rt_expr x, rt_expr y) override { assert(0); }
  rt_expr f_gen_slice(rt_expr e, bigint lo, bigint wd) override { assert(0); }

  rt_expr f_gen_FPCompare(rt_expr x, rt_expr y, rt_expr signalnan, rt_expr fpcr) override { assert(0); }
  rt_expr f_gen_FPCompareEQ(rt_expr x, rt_expr y, rt_expr fpcr) override { assert(0); }
  rt_expr f_gen_FPCompareGE(rt_expr x, rt_expr y, rt_expr fpcr) override { assert(0); }
  rt_expr f_gen_FPCompareGT(rt_expr x, rt_expr y, rt_expr fpcr) override { assert(0); }
  rt_expr f_gen_FPAdd(rt_expr x, rt_expr y, rt_expr fpcr) override { assert(0); }
  rt_expr f_gen_FPSub(rt_expr x, rt_expr y, rt_expr fpcr) override { assert(0); }
  rt_expr f_gen_FPMulAdd(rt_expr addend, rt_expr x, rt_expr y, rt_expr fpcr) override { assert(0); }
  rt_expr f_gen_FPMulAddH(rt_expr addend, rt_expr x, rt_expr y, rt_expr fpcr) override { assert(0); }
  rt_expr f_gen_FPMulX(rt_expr x, rt_expr y, rt_expr fpcr) override { assert(0); }
  rt_expr f_gen_FPMul(rt_expr x, rt_expr y, rt_expr fpcr) override { assert(0); }
  rt_expr f_gen_FPDiv(rt_expr x, rt_expr y, rt_expr fpcr) override { assert(0); }
  rt_expr f_gen_FPMin(rt_expr x, rt_expr y, rt_expr fpcr) override { assert(0); }
  rt_expr f_gen_FPMinNum(rt_expr x, rt_expr y, rt_expr fpcr) override { assert(0); }
  rt_expr f_gen_FPMax(rt_expr x, rt_expr y, rt_expr fpcr) override { assert(0); }
  rt_expr f_gen_FPMaxNum(rt_expr x, rt_expr y, rt_expr fpcr) override { assert(0); }
  rt_expr f_gen_FPRecpX(rt_expr x, rt_expr fpcr) override { assert(0); }
  rt_expr f_gen_FPSqrt(rt_expr x, rt_expr fpcr) override { assert(0); }
  rt_expr f_gen_FPRecipEstimate(rt_expr x, rt_expr fpcr) override { assert(0); }
  rt_expr f_gen_BFAdd(rt_expr x, rt_expr y) override { assert(0); }
  rt_expr f_gen_BFMul(rt_expr x, rt_expr y) override { assert(0); }
  rt_expr f_gen_FPConvertBF(rt_expr x, rt_expr fpcr, rt_expr rounding) override { assert(0); }
  rt_expr f_gen_FPRecipStepFused(rt_expr x, rt_expr y) override { assert(0); }
  rt_expr f_gen_FPRSqrtStepFused(rt_expr x, rt_expr y) override { assert(0); }
  rt_expr f_gen_FPToFixed(rt_expr x, rt_expr fbits, rt_expr isunsigned, rt_expr fpcr, rt_expr rounding) override { assert(0); }
  rt_expr f_gen_FixedToFP(rt_expr x, rt_expr fbits, rt_expr isunsigned, rt_expr fpcr, rt_expr rounding) override { assert(0); }
  rt_expr f_gen_FPConvert(rt_expr x, rt_expr fpcr, rt_expr rounding) override { assert(0); }
  rt_expr f_gen_FPRoundInt(rt_expr x, rt_expr fpcr, rt_expr rounding, rt_expr isexact) override { assert(0); }
  rt_expr f_gen_FPRoundIntN(rt_expr x, rt_expr fpcr, rt_expr rounding, rt_expr intsize) override { assert(0); }
  rt_expr f_gen_FPToFixedJS_impl(rt_expr x, rt_expr fpcr, rt_expr is64) override { assert(0); } // from override.asl
};


class llvm_lifter_interface : public llvm_lift_time_interface, public llvm_run_time_interface { };

static_assert(!std::is_abstract_v<llvm_lifter_interface>);

} // namespace aslp
