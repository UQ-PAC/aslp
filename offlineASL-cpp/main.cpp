#include <iostream>

#include "llvm/ADT/ArrayRef.h"

#include "aslp_lifter.hpp"
#include "llvm_interface.hpp"

int main(int argc, char **argv) {
  llvm::LLVMContext context;
  auto module = llvm::Module{"aslp_lifter_module", context};

  auto funty = llvm::FunctionType::get(llvm::Type::getVoidTy(context), {}, false);
  auto function = llvm::Function::Create(funty, llvm::GlobalValue::LinkageTypes::ExternalLinkage, "aslp_lifter_fun", module);
  auto entry = llvm::BasicBlock::Create(context, "entry", function);

  aslp::llvm_lifter_interface iface{*function};
  aslp::aslp_lifter<aslp::llvm_lifter_traits> lifter{iface};

  unsigned long long adds = 0xab030041;
  auto v_enc = llvm::APInt{32, adds};
  lifter.f_A64_decoder(v_enc);

  module.print(llvm::errs(), nullptr);
  // module.dump();

  std::cout << "boop" << std::endl;
}
