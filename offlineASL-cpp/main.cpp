#include <iostream>

#include "aslp_lifter_impl.hpp"
#include "llvm_interface.hpp"

int main(int argc, char **argv) {
  aslp::llvm_lifter_interface iface{};
  aslp::aslp_lifter<aslp::llvm_lifter_traits> lifter{iface};
  unsigned long long adds = 0xab030041;
  auto v_enc = llvm::APInt{32, adds};
  lifter.f_A64_decoder(v_enc);

  aslp::llvm_lifter_interface::rt_expr v_Exp68__2 = iface.f_gen_FPMulAdd(
      iface.f_gen_append_bits(
          iface.f_gen_not_bits(iface.f_gen_slice(
              iface.f_gen_array_load(iface.v__Z(),
                                     iface.f_cvt_bits_uint(iface.extract_bits(
                                         v_enc, /*lo*/ iface.bigint_lit("10"),
                                         /*wd*/ iface.bigint_lit("5")))),
              iface.bigint_lit("63"), iface.bigint_lit("1"))),
          iface.f_gen_slice(
              iface.f_gen_array_load(iface.v__Z(),
                                     iface.f_cvt_bits_uint(iface.extract_bits(
                                         v_enc, /*lo*/ iface.bigint_lit("10"),
                                         /*wd*/ iface.bigint_lit("5")))),
              iface.bigint_lit("0"), iface.bigint_lit("63"))),
      iface.f_gen_slice(
          iface.f_gen_array_load(iface.v__Z(),
                                 iface.f_cvt_bits_uint(iface.extract_bits(
                                     v_enc, /*lo*/ iface.bigint_lit("5"),
                                     /*wd*/ iface.bigint_lit("5")))),
          iface.bigint_lit("0"), iface.bigint_lit("64")),
      iface.f_gen_slice(
          iface.f_gen_array_load(iface.v__Z(),
                                 iface.f_cvt_bits_uint(iface.extract_bits(
                                     v_enc, /*lo*/ iface.bigint_lit("16"),
                                     /*wd*/ iface.bigint_lit("5")))),
          iface.bigint_lit("0"), iface.bigint_lit("64")),
      iface.f_gen_load(iface.v_FPCR()));

  std::cout << "boop" << std::endl;
}
