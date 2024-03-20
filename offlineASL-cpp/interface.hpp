#pragma once

#include <llvm/MC/MCExpr.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>

namespace aslp {

// an opcode for ASLP is four bytes (= 32 bits), in Arm's conventional little-endian order.
using opcode_t = std::array<uint8_t, 4>;

enum struct err_t {
  missing,
  banned, 
};

enum struct pstate_t : uint64_t {
  N = 0, Z, C, V
};

enum struct reg_t {
  X,
  V,
  PSTATE,
};

template<typename TYPE, typename EXPR, typename LEXPR, typename STMT, typename SLICE>
class lifter_interface {
public:

  using type_t = TYPE;
  using expr_t = EXPR;
  using lexpr_t = LEXPR;
  using stmt_t = STMT;
  using slice_t = SLICE;

  virtual ~lifter_interface() = default;

  // perform possibly non-atomic memory load/store  
  // virtual llvm::Value* mem_load(llvm::Value* addr, llvm::Value* size) = 0;
  // virtual void mem_store(llvm::Value* addr, llvm::Value* size, llvm::Value* rhs) = 0;

  // should return a ptr type value suitable for load and store
  virtual lexpr_t get_reg(reg_t, uint64_t num) = 0;

  // XXX: callback for `aslt_visitor` to inform arm2llvm of changed basic blocks,
  // so create* functions create instructions in the right place.
  virtual void set_bb(llvm::BasicBlock*) = 0;
  virtual llvm::BasicBlock* get_bb() = 0;

  virtual llvm::Function& ll_function() = 0;

  // lifted instructions are named using the number of the ARM
  // instruction they come from
  virtual std::string nextName() = 0;

  virtual expr_t lookupExprVar(const llvm::MCExpr&) = 0;
  virtual void updateOutputReg(expr_t V, bool SExt = false) = 0;

  virtual expr_t getIntConst(uint64_t val, u_int64_t bits) = 0;
  virtual type_t getIntTy(unsigned bits) = 0; 
  virtual type_t getFPType(unsigned bits) = 0; 

  virtual void assertTrue(expr_t cond) = 0;

  virtual expr_t makeLoadWithOffset(expr_t base, expr_t offset, int size) = 0; 
  virtual void storeToMemoryValOffset(expr_t base, expr_t offset, u_int64_t size, expr_t val) = 0;

  virtual lexpr_t createAlloca(type_t ty, expr_t sz, const std::string &NameStr) = 0;

  virtual expr_t createGEP(type_t ty, expr_t v, llvm::ArrayRef<expr_t> idxlist,
                               const std::string &NameStr) = 0;

  virtual void createBranch(expr_t c, stmt_t t, stmt_t f) = 0;

  virtual void createBranch(stmt_t dst) = 0;

  virtual expr_t createLoad(type_t ty, expr_t ptr) = 0;

  virtual void createStore(expr_t v, expr_t ptr) = 0;

  virtual expr_t createTrap() = 0;

  virtual expr_t createSMin(expr_t a, expr_t b) = 0;

  virtual expr_t createSMax(expr_t a, expr_t b) = 0;

  virtual expr_t createUMin(expr_t a, expr_t b) = 0;

  virtual expr_t createUMax(expr_t a, expr_t b) = 0;

  virtual expr_t createFNeg(expr_t v) = 0;

  virtual expr_t createFAbs(expr_t v) = 0;

  virtual expr_t createSSubOverflow(expr_t a, expr_t b) = 0;

  virtual expr_t createSAddOverflow(expr_t a, expr_t b) = 0;

  virtual expr_t createUSubOverflow(expr_t a, expr_t b) = 0;

  virtual expr_t createUAddOverflow(expr_t a, expr_t b) = 0;

  virtual expr_t createUAddSat(expr_t a, expr_t b) = 0;

  virtual expr_t createUSubSat(expr_t a, expr_t b) = 0;

  virtual expr_t createSAddSat(expr_t a, expr_t b) = 0;

  virtual expr_t createSSubSat(expr_t a, expr_t b) = 0;

  virtual expr_t createCtPop(expr_t v) = 0;

  // first argument is an i16
  virtual expr_t createConvertFromFP16(expr_t v, type_t ty) = 0;

  virtual expr_t createConvertFPToSI(expr_t v, type_t ty) = 0;

  virtual expr_t createConvertFPToUI(expr_t v, type_t ty) = 0;

  virtual expr_t createPtrToInt(expr_t v, type_t ty) = 0;

  virtual expr_t createInsertElement(expr_t vec, expr_t val, int idx) = 0;

  virtual expr_t createExtractElement(expr_t v, expr_t idx) = 0;

  virtual expr_t createExtractElement(expr_t v, int idx) = 0;

  virtual expr_t createShuffleVector(expr_t v, llvm::ArrayRef<int> mask) = 0;

  virtual expr_t getIndexedElement(unsigned idx, unsigned eltSize, unsigned reg) = 0;

  virtual expr_t createExtractValue(expr_t v, llvm::ArrayRef<unsigned> idxs) = 0;

  virtual expr_t createReturn(expr_t v) = 0;

  virtual expr_t createFShr(expr_t a, expr_t b, expr_t c) = 0;

  virtual expr_t createFShl(expr_t a, expr_t b, expr_t c) = 0;

  virtual expr_t createBitReverse(expr_t v) = 0;

  virtual expr_t createAbs(expr_t v) = 0;

  virtual expr_t createCtlz(expr_t v) = 0;

  virtual expr_t createBSwap(expr_t v) = 0;

  virtual expr_t createVectorReduceAdd(expr_t v) = 0;

  virtual expr_t createSelect(expr_t cond, expr_t a, expr_t b) = 0;

  virtual expr_t createICmp(llvm::ICmpInst::Predicate p, expr_t a, expr_t b) = 0;

  virtual expr_t createFCmp(llvm::FCmpInst::Predicate p, expr_t a, expr_t b) = 0;

  virtual expr_t createBinop(expr_t a, expr_t b, llvm::Instruction::BinaryOps op) = 0;

  virtual expr_t createUDiv(expr_t a, expr_t b) = 0;

  virtual expr_t createSDiv(expr_t a, expr_t b) = 0;

  virtual expr_t createMul(expr_t a, expr_t b) = 0;

  virtual expr_t createAdd(expr_t a, expr_t b) = 0;

  virtual expr_t createSub(expr_t a, expr_t b) = 0;

  virtual expr_t createRawLShr(expr_t a, expr_t b) = 0;

  virtual expr_t createMaskedLShr(expr_t a, expr_t b) = 0;

  virtual expr_t createRawAShr(expr_t a, expr_t b) = 0;

  virtual expr_t createMaskedAShr(expr_t a, expr_t b) = 0;

  virtual expr_t createRawShl(expr_t a, expr_t b) = 0;

  virtual expr_t createMaskedShl(expr_t a, expr_t b) = 0;

  virtual expr_t getLowOnes(int ones, int w) = 0;

  virtual expr_t createMSL(expr_t a, int b) = 0;

  virtual expr_t createAnd(expr_t a, expr_t b) = 0;

  virtual expr_t createOr(expr_t a, expr_t b) = 0;

  virtual expr_t createXor(expr_t a, expr_t b) = 0;

  virtual expr_t createNot(expr_t a) = 0;

  virtual expr_t createFreeze(expr_t v) = 0;

  virtual expr_t createTrunc(expr_t v, type_t t) = 0;

  virtual expr_t createSExt(expr_t v, type_t t) = 0;

  virtual expr_t createZExt(expr_t v, type_t t) = 0;

  virtual expr_t createUIToFP(expr_t v, type_t t) = 0;
  virtual expr_t createSIToFP(expr_t v, type_t t) = 0;

  virtual expr_t createBitCast(expr_t v, type_t t) = 0;

  virtual expr_t createCast(expr_t v, type_t t, llvm::Instruction::CastOps op) = 0;

};


namespace detail {
  using type_t = llvm::Type *;
  using expr_t = llvm::Value *;
  using lexpr_t = llvm::AllocaInst *;
  using stmt_t = std::pair<llvm::BasicBlock*, llvm::BasicBlock*>;
  struct slice_t {
    unsigned lo;
    unsigned wd;
  };

  using lifter_interface_llvm = lifter_interface<type_t, expr_t, lexpr_t, stmt_t, slice_t>;
}

using detail::lifter_interface_llvm;


}
