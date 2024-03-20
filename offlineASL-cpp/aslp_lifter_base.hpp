#pragma once

#include <cassert>
#include <vector>

#include <llvm/ADT/APInt.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/BasicBlock.h>

namespace aslp {

using bits = llvm::APInt;
using bigint = long long;

bits extract_bits(const bits &val, bigint lo, bigint wd) {
  return val.extractBits(wd, lo);
}

bool f_eq_bits(bits x, bits y) {
  return x == y;
}
bool f_ne_bits(bits x, bits y) {
  return x != y;
}
bits f_add_bits(bits x, bits y) {
  return x + y;
}
bits f_sub_bits(bits x, bits y) {
  return x - y;
}
bits f_mul_bits(bits x, bits y) {
  return x * y;
}
bits f_and_bits(bits x, bits y) {
  return x & y;
}
bits f_or_bits(bits x, bits y) {
  return x | y;
}
bits f_eor_bits(bits x, bits y) {
  return x ^ y;
}
bits f_not_bits(bits x) {
  return ~x;
}
bool f_slt_bits(bits x, bits y) {
  return x.slt(y);
}
bool f_sle_bits(bits x, bits y) {
  return x.sle(y);
}
bits f_zeros_bits(bigint n) {
  return llvm::APInt::getZero(n);
}
bits f_ones_bits(bigint n) {
  return llvm::APInt::getAllOnes(n);
}
bits f_replicate_bits(bits x, bigint n) {
  assert(n >= 1);
  bits original = x;
  unsigned wd = x.getBitWidth();
  x = x.zextOrTrunc(wd * n);
  for (unsigned i = 1; i < n; i++) {
    x <<= wd;
    x |= original;
  }
  return x;
}
bits f_append_bits(bits x, bits y) {
  x = x.zext(x.getBitWidth() + y.getBitWidth());
  x <<= y.getBitWidth();
  x |= y;
  return x;
}
bits f_ZeroExtend(bits x, bigint wd) {
  return x.zextOrTrunc(wd);
}
bits f_SignExtend(bits x, bigint wd) {
  return x.sextOrTrunc(wd);
}
bits f_lsl_bits(bits x, bits y) {
  return x << y;
}
bits f_lsr_bits(bits x, bits y) {
  return x.lshr(y);
}
bits f_asr_bits(bits x, bits y) {
  return x.ashr(y);
}
bigint f_cvt_bits_uint(bits x) {
  return x.getZExtValue();
}


class aslp_lifter_base {

};

};