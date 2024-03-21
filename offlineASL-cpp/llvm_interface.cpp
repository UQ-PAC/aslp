#include <llvm/ADT/APInt.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Instructions.h>

#include "interface.hpp"

namespace aslp {

using  __lifter_interface_llvm = lifter_interface<llvm::APInt, long long, llvm::Value *, llvm::AllocaInst *, llvm::BasicBlock *>;

class llvm_lifter_interface : public __lifter_interface_llvm {

  using super = __lifter_interface_llvm;
  using typename super::bits;
  using typename super::bigint;
  using typename super::rt_expr;
  using typename super::rt_lexpr;
  using typename super::rt_label;

  virtual bits extract_bits(const bits &val, bigint lo, bigint wd) override {
    return val.extractBits(wd, lo);
  }
  bool f_eq_bits(const bits &x, const bits &y) override {
    return x == y;
  }
  bool f_ne_bits(const bits &x, const bits &y) override {
    return x != y;
  }
  virtual bits f_add_bits(const bits &x, const bits &y) override {
    return x + y;
  }
  virtual bits f_sub_bits(const bits &x, const bits &y) override {
    return x - y;
  }
  virtual bits f_mul_bits(const bits &x, const bits &y) override {
    return x * y;
  }
  virtual bits f_and_bits(const bits &x, const bits &y) override {
    return x & y;
  }
  virtual bits f_or_bits(const bits &x, const bits &y) override {
    return x | y;
  }
  virtual bits f_eor_bits(const bits &x, const bits &y) override {
    return x ^ y;
  }
  virtual bits f_not_bits(const bits &x) override {
    return ~x;
  }
  bool f_slt_bits(const bits &x, const bits &y) override {
    return x.slt(y);
  }
  bool f_sle_bits(const bits &x, const bits &y) override {
    return x.sle(y);
  }
  virtual bits f_zeros_bits(bigint n) override {
    return llvm::APInt::getZero(n);
  }
  virtual bits f_ones_bits(bigint n) override {
    return llvm::APInt::getAllOnes(n);
  }
  virtual bits f_replicate_bits(const bits &x, bigint n) override {
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
  virtual bits f_append_bits(const bits &x, const bits &y) override {
    auto ret = x.zext(x.getBitWidth() + y.getBitWidth());
    ret <<= y.getBitWidth();
    ret |= y;
    return ret;
  }
  virtual bits f_ZeroExtend(const bits &x, bigint wd) override {
    return x.zextOrTrunc(wd);
  }
  virtual bits f_SignExtend(const bits &x, bigint wd) override {
    return x.sextOrTrunc(wd);
  }
  virtual bits f_lsl_bits(const bits &x, const bits &y) override {
    return x << y;
  }
  virtual bits f_lsr_bits(const bits &x, const bits &y) override {
    return x.lshr(y);
  }
  virtual bits f_asr_bits(const bits &x, const bits &y) override {
    return x.ashr(y);
  }
  bigint f_cvt_bits_uint(const bits &x) override {
    return x.getZExtValue();
  }

};

} // namespace aslp
