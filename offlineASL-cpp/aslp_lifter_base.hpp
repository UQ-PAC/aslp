#pragma once

#include <cassert>
#include <vector>

#include <llvm/ADT/APInt.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/BasicBlock.h>

namespace aslp {

// bits which are known at lift-time
using bits = llvm::APInt;
// bigints which are known at lift-time
using bigint = long long;

// runtime-expression type, i.e. the type of values produced by the semantics
using rt_expr = llvm::Value *;
// runtime-label, supports switching blocks during semantics generation
using rt_label = llvm::BasicBlock *;

bits extract_bits(const bits &val, bigint lo, bigint wd);

bool f_eq_bits(bits x, bits y);
bool f_ne_bits(bits x, bits y);
bits f_add_bits(bits x, bits y);
bits f_sub_bits(bits x, bits y);
bits f_mul_bits(bits x, bits y);
bits f_and_bits(bits x, bits y);
bits f_or_bits(bits x, bits y);
bits f_eor_bits(bits x, bits y);
bits f_not_bits(bits x);
bool f_slt_bits(bits x, bits y);
bool f_sle_bits(bits x, bits y);
bits f_zeros_bits(bigint n);
bits f_ones_bits(bigint n);
bits f_replicate_bits(bits x, bigint n);
bits f_append_bits(bits x, bits y);
bits f_ZeroExtend(bits x, bigint wd);
bits f_SignExtend(bits x, bigint wd);
bits f_lsl_bits(bits x, bits y);
bits f_lsr_bits(bits x, bits y);
bits f_asr_bits(bits x, bits y);
bigint f_cvt_bits_uint(bits x);

class aslp_lifter_base {

virtual rt_expr f_decl_bv(const std::string_view& name, bigint width) = 0;
let f_decl_bool name =

let f_switch_context ctx =

let f_gen_branch cond =
let f_gen_assert b =
let f_gen_bit_lit w (bv: bitvector) =
let f_gen_bool_lit b =
let f_gen_int_lit i =
let f_gen_load v = v
let f_gen_store v e = push_stmt (Stmt_Assign(to_lexpr v, e, loc))
let f_gen_array_load a i =
let f_gen_array_store a i e =
let f_gen_Mem_set w x _ y z =
let f_gen_Mem_read w x _ y =
let f_gen_AArch64_MemTag_set x y z: unit =
let f_gen_and_bool e1 e2 =
let f_gen_or_bool e1 e2 =
let f_gen_not_bool e1 =
let f_gen_eq_enum e1 e2 =
let f_gen_cvt_bits_uint w x =
let f_gen_eq_bits w e1 e2 =
let f_gen_ne_bits w e1 e2 =
let f_gen_not_bits w e1 =
let f_gen_cvt_bool_bv e =
let f_gen_or_bits w e1 e2 =
let f_gen_eor_bits w e1 e2 =
let f_gen_and_bits w e1 e2 =
let f_gen_add_bits w e1 e2 =
let f_gen_sub_bits w e1 e2 =
let f_gen_sdiv_bits w e1 e2 =
let f_gen_sle_bits w e1 e2 =
let f_gen_slt_bits w e1 e2 =
let f_gen_mul_bits w e1 e2 =
let f_gen_append_bits xw yw x y =
let f_gen_lsr_bits xw yw x y =
let f_gen_lsl_bits xw yw x y =
let f_gen_asr_bits xw yw x y =
let f_gen_replicate_bits xw yw x y =
let f_gen_ZeroExtend xw yw x y =
let f_gen_SignExtend xw yw x y =
let f_gen_slice e lo wd =
let f_gen_FPCompare w x y s t =
let f_gen_FPCompareEQ w x y r =
let f_gen_FPCompareGE w x y r =
let f_gen_FPCompareGT w x y r =
let f_gen_FPAdd w x y r =
let f_gen_FPSub w x y r =
let f_gen_FPMulAdd w a x y r =
let f_gen_FPMulAddH w a x y r =
let f_gen_FPMulX w x y r =
let f_gen_FPMul w x y r =
let f_gen_FPDiv w x y r =
let f_gen_FPMin w x y r =
let f_gen_FPMinNum w x y r =
let f_gen_FPMax w x y r =
let f_gen_FPMaxNum w x y r =
let f_gen_FPRecpX w x t =
let f_gen_FPSqrt w x t =
let f_gen_FPRecipEstimate w x r =
let f_gen_BFAdd x y =
let f_gen_BFMul x y =
let f_gen_FPConvertBF x t r =
let f_gen_FPRecipStepFused w x y =
let f_gen_FPRSqrtStepFused w x y =
let f_gen_FPToFixed w w' x b u t r =
let f_gen_FixedToFP w w' x b u t r =
let f_gen_FPConvert w w' x t r =
let f_gen_FPRoundInt w x t r e =
let f_gen_FPRoundIntN w x t r e =
let f_gen_FPToFixedJS_impl w w' x t s =


};

};