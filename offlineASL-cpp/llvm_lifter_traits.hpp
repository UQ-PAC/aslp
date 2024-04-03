#include <interface.hpp>

#include <llvm/ADT/APInt.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>

#include <variant>

namespace aslp {

struct llvm_lifter_traits {
  using bits = llvm::APInt;
  using bigint = long long;
  using rt_expr = llvm::Value *;

  using rt_local = llvm::AllocaInst *;
  using rt_global = std::shared_ptr<std::vector<llvm::GlobalVariable *>>;

  using rt_lexpr = std::variant<rt_local, rt_global>;
  using rt_label = std::shared_ptr<llvm::IRBuilder<>>;
};

static_assert(lifter_traits<llvm_lifter_traits>);

} // namespace aslp
