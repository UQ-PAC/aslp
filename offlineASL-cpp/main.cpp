#include <iostream>

#include "aslp_lifter.hpp"
#include "llvm_interface.hpp"


int main(int argc, char** argv) {
  aslp::llvm_lifter_interface iface{};
  aslp::aslp_lifter<aslp::llvm_lifter_traits> lifter{iface};
  unsigned long long adds = 0xab030041;
  lifter.f_A64_decoder(llvm::APInt{32, adds});

  std::cout << "boop" << std::endl;
}
