#include <iostream>

#include "aslp_lifter.hpp"
#include "llvm_interface.hpp"


int main(int argc, char** argv) {
  aslp::llvm_lifter_interface iface{};
  aslp::aslp_lifter<aslp::llvm_lifter_traits> lifter{iface};
  lifter.f_A64_decoder(llvm::APInt{32, 0xe205800ULL});

  std::cout << "boop" << std::endl;
}
