#include <iostream>

#include "llvm/ADT/ArrayRef.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

#include <aslp/aslp_lifter.hpp>
#ifdef ASLP_LIFTER_IMPL
#include <aslp/aslp_lifter_impl.hpp>
#endif

#include <aslp/llvm_interface.hpp>

int main(int argc, char **argv) {
  llvm::LLVMContext context;

  llvm::Module module{"aslp_lifter_module", context};

  auto funty = llvm::FunctionType::get(llvm::Type::getVoidTy(context), {}, false);
  auto function = llvm::Function::Create(funty, llvm::GlobalValue::LinkageTypes::ExternalLinkage, "aslp_lifter_fun", module);
  auto entry = llvm::BasicBlock::Create(context, "entry", function);

  aslp::llvm_lifter_interface iface{*function};
  aslp::aslp_lifter<aslp::llvm_lifter_traits> lifter{iface};

  assert(argc > 1);
  char* endp;
  unsigned long long adds = std::strtoull(argv[1], &endp, 0);
  assert(endp == argv[1] + strlen(argv[1]));
  auto v_enc = llvm::APInt{32, adds};
  lifter.f_A64_decoder(v_enc);

  module.print(llvm::errs(), nullptr);
  // module.dump();

  assert(llvm::verifyModule(module));

  std::cout << "boop! module validated." << std::endl;
}
