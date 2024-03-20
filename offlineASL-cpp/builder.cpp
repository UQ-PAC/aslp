#include "interface.hpp"

#include <iostream>

#include "llvm/IR/Module.h"
#include "llvm/IR/Intrinsics.h"
// #include "llvm/MC/MCCodeEmitter.h"
// #include "llvm/MC/MCInst.h"
// #include "llvm/MC/MCFixup.h"

class arm2llvm : public aslp::lifter_interface_llvm {
  llvm::Module *LiftedModule;
  llvm::LLVMContext &Ctx{LiftedModule->getContext()};
  llvm::BasicBlock *LLVMBB{nullptr};

  llvm::Constant *getIntConst(uint64_t val, u_int64_t bits) override {
    return llvm::ConstantInt::get(Ctx, llvm::APInt(bits, val));
  }

  llvm::Type *getIntTy(unsigned bits) override {
    return llvm::Type::getIntNTy(Ctx, bits);
  }

  llvm::Type *getFPType(unsigned bits) override {
    if (bits == 16)
      return llvm::Type::getHalfTy(Ctx);
    else if (bits == 32)
      return llvm::Type::getFloatTy(Ctx);
    else if (bits == 64)
      return llvm::Type::getDoubleTy(Ctx);
    else
      assert(false && "unsupported floating point type");
  }

  // llvm::AllocaInst* get_reg(aslp::reg_t regtype, uint64_t num) override {
  //   using namespace aslp; // reg_t and pstate_t
  //
  //   uint64_t reg = 0;
  //   if (regtype == reg_t::X) {
  //     if (num <= 28)
  //       reg = llvm::AArch64::X0 + num;
  //     else if (num == 29)
  //       reg = llvm::AArch64::FP;
  //     else if (num == 30)
  //       reg = llvm::AArch64::LR;
  //     else if (num == 31)
  //       reg = llvm::AArch64::SP;
  //     else
  //       assert(false && "X register out of range");
  //
  //   } else if (regtype == reg_t::PSTATE) {
  //
  //     if (num == (int)pstate_t::N) reg = llvm::AArch64::N;
  //     else if (num == (int)pstate_t::Z) reg = llvm::AArch64::Z;
  //     else if (num == (int)pstate_t::C) reg = llvm::AArch64::C;
  //     else if (num == (int)pstate_t::V) reg = llvm::AArch64::V;
  //
  //   } else if (regtype == reg_t::V) {
  //     reg = llvm::AArch64::Q0 + num;
  //
  //   }
  //
  //   assert(reg && "register not mapped");
  //   return llvm::cast<llvm::AllocaInst>(RegFile.at(reg));
  // }

  void set_bb(llvm::BasicBlock * bb) override {
    LLVMBB = bb;
  }
  llvm::BasicBlock* get_bb() override {
    return LLVMBB;
  }

  // llvm::Function& ll_function() override {
  //   return *liftedFn;
  // }

  // lifted instructions are named using the number of the ARM
  // instruction they come from
  // std::string nextName() override {
  //   std::stringstream ss;
  //   ss << "a" << armInstNum << "_" << llvmInstNum++;
  //   return ss.str();
  // }

  llvm::AllocaInst *createAlloca(llvm::Type *ty, llvm::Value *sz, const std::string &NameStr) override {
    return new llvm::AllocaInst(ty, 0, sz, NameStr, LLVMBB);
  }

  llvm::GetElementPtrInst *createGEP(llvm::Type *ty, llvm::Value *v, llvm::ArrayRef<llvm::Value *> idxlist,
                               const std::string &NameStr) override {
    return llvm::GetElementPtrInst::Create(ty, v, idxlist, NameStr, LLVMBB);
  }

  void createBranch(llvm::Value *c, llvm::BasicBlock *t, llvm::BasicBlock *f) {
    llvm::BranchInst::Create(t, f, c, LLVMBB);
  }

  void createBranch(llvm::Value *c, stmt_t t, stmt_t f) override {
   createBranch(c, t.first, f.first);
  }

  void createBranch(llvm::BasicBlock *dst) {
    llvm::BranchInst::Create(dst, LLVMBB);
  }

  void createBranch(stmt_t dst) override {
    createBranch(dst.first);
  }

  llvm::LoadInst *createLoad(llvm::Type *ty, llvm::Value *ptr) override {
    return new llvm::LoadInst(ty, ptr, nextName(), false, llvm::Align(1), LLVMBB);
  }

  void createStore(llvm::Value *v, llvm::Value *ptr) override {
    new llvm::StoreInst(v, ptr, false, llvm::Align(1), LLVMBB);
  }

  llvm::Value *createTrap() override {
    auto decl = llvm::Intrinsic::getDeclaration(LiftedModule, llvm::Intrinsic::trap);
    return llvm::CallInst::Create(decl, "", LLVMBB);
  }

  llvm::Value *createSMin(llvm::Value *a, llvm::Value *b) override {
    auto decl =
        llvm::Intrinsic::getDeclaration(LiftedModule, llvm::Intrinsic::smin, a->getType());
    return llvm::CallInst::Create(decl, {a, b}, nextName(), LLVMBB);
  }

  llvm::Value *createSMax(llvm::Value *a, llvm::Value *b) override {
    auto decl =
        llvm::Intrinsic::getDeclaration(LiftedModule, llvm::Intrinsic::smax, a->getType());
    return llvm::CallInst::Create(decl, {a, b}, nextName(), LLVMBB);
  }

  llvm::Value *createUMin(llvm::Value *a, llvm::Value *b) override {
    auto decl =
        llvm::Intrinsic::getDeclaration(LiftedModule, llvm::Intrinsic::umin, a->getType());
    return llvm::CallInst::Create(decl, {a, b}, nextName(), LLVMBB);
  }

  llvm::Value *createUMax(llvm::Value *a, llvm::Value *b) override {
    auto decl =
        llvm::Intrinsic::getDeclaration(LiftedModule, llvm::Intrinsic::umax, a->getType());
    return llvm::CallInst::Create(decl, {a, b}, nextName(), LLVMBB);
  }

  llvm::Value *createFNeg(llvm::Value *v) override {
    return llvm::UnaryOperator::CreateFNeg(v, nextName(), LLVMBB);
  }

  llvm::Value *createFAbs(llvm::Value *v) override {
    auto fabs_decl =
        llvm::Intrinsic::getDeclaration(LiftedModule, llvm::Intrinsic::fabs, v->getType());
    return llvm::CallInst::Create(fabs_decl, {v}, nextName(), LLVMBB);
  }

  llvm::CallInst *createSSubOverflow(llvm::Value *a, llvm::Value *b) override {
    auto ssub_decl = llvm::Intrinsic::getDeclaration(
        LiftedModule, llvm::Intrinsic::ssub_with_overflow, a->getType());
    return llvm::CallInst::Create(ssub_decl, {a, b}, nextName(), LLVMBB);
  }

  llvm::CallInst *createSAddOverflow(llvm::Value *a, llvm::Value *b) override {
    auto sadd_decl = llvm::Intrinsic::getDeclaration(
        LiftedModule, llvm::Intrinsic::sadd_with_overflow, a->getType());
    return llvm::CallInst::Create(sadd_decl, {a, b}, nextName(), LLVMBB);
  }

  llvm::CallInst *createUSubOverflow(llvm::Value *a, llvm::Value *b) override {
    auto usub_decl = llvm::Intrinsic::getDeclaration(
        LiftedModule, llvm::Intrinsic::usub_with_overflow, a->getType());
    return llvm::CallInst::Create(usub_decl, {a, b}, nextName(), LLVMBB);
  }

  llvm::CallInst *createUAddOverflow(llvm::Value *a, llvm::Value *b) override {
    auto uadd_decl = llvm::Intrinsic::getDeclaration(
        LiftedModule, llvm::Intrinsic::uadd_with_overflow, a->getType());
    return llvm::CallInst::Create(uadd_decl, {a, b}, nextName(), LLVMBB);
  }

  llvm::CallInst *createUAddSat(llvm::Value *a, llvm::Value *b) override {
    auto uadd_decl = llvm::Intrinsic::getDeclaration(
        LiftedModule, llvm::Intrinsic::uadd_sat, a->getType());
    return llvm::CallInst::Create(uadd_decl, {a, b}, nextName(), LLVMBB);
  }

  llvm::CallInst *createUSubSat(llvm::Value *a, llvm::Value *b) override {
    auto usub_decl = llvm::Intrinsic::getDeclaration(
        LiftedModule, llvm::Intrinsic::usub_sat, a->getType());
    return llvm::CallInst::Create(usub_decl, {a, b}, nextName(), LLVMBB);
  }

  llvm::CallInst *createSAddSat(llvm::Value *a, llvm::Value *b) override {
    auto sadd_decl = llvm::Intrinsic::getDeclaration(
        LiftedModule, llvm::Intrinsic::sadd_sat, a->getType());
    return llvm::CallInst::Create(sadd_decl, {a, b}, nextName(), LLVMBB);
  }

  llvm::CallInst *createSSubSat(llvm::Value *a, llvm::Value *b) override {
    auto ssub_decl = llvm::Intrinsic::getDeclaration(
        LiftedModule, llvm::Intrinsic::ssub_sat, a->getType());
    return llvm::CallInst::Create(ssub_decl, {a, b}, nextName(), LLVMBB);
  }

  llvm::CallInst *createCtPop(llvm::Value *v) override {
    auto decl =
        llvm::Intrinsic::getDeclaration(LiftedModule, llvm::Intrinsic::ctpop, v->getType());
    return llvm::CallInst::Create(decl, {v}, nextName(), LLVMBB);
  }

  // first argument is an i16
  llvm::CallInst *createConvertFromFP16(llvm::Value *v, llvm::Type *ty) override {
    auto cvt_decl = llvm::Intrinsic::getDeclaration(LiftedModule,
                                              llvm::Intrinsic::convert_from_fp16, ty);
    return llvm::CallInst::Create(cvt_decl, {v}, nextName(), LLVMBB);
  }

  llvm::CastInst *createConvertFPToSI(llvm::Value *v, llvm::Type *ty) override {
    return new llvm::FPToSIInst(v, ty, nextName(), LLVMBB);
  }

  llvm::CastInst *createConvertFPToUI(llvm::Value *v, llvm::Type *ty) override {
    return new llvm::FPToUIInst(v, ty, nextName(), LLVMBB);
  }

  llvm::CastInst *createPtrToInt(llvm::Value *v, llvm::Type *ty) override {
    return new llvm::PtrToIntInst(v, ty, nextName(), LLVMBB);
  }

  llvm::InsertElementInst *createInsertElement(llvm::Value *vec, llvm::Value *val, int idx) override {
    auto idxv = getIntConst(idx, 32);
    return llvm::InsertElementInst::Create(vec, val, idxv, nextName(), LLVMBB);
  }

  llvm::ExtractElementInst *createExtractElement(llvm::Value *v, llvm::Value *idx) override {
    return llvm::ExtractElementInst::Create(v, idx, nextName(), LLVMBB);
  }

  llvm::ExtractElementInst *createExtractElement(llvm::Value *v, int idx) override {
    auto idxv = getIntConst(idx, 32);
    return llvm::ExtractElementInst::Create(v, idxv, nextName(), LLVMBB);
  }

  llvm::ShuffleVectorInst *createShuffleVector(llvm::Value *v, llvm::ArrayRef<int> mask) override {
    return new llvm::ShuffleVectorInst(v, mask, nextName(), LLVMBB);
  }

  llvm::ShuffleVectorInst *createShuffleVector(llvm::Value *v, llvm::Value *mask) {
    return new llvm::ShuffleVectorInst(v, mask, nextName(), LLVMBB);
  }

  llvm::ExtractValueInst *createExtractValue(llvm::Value *v, llvm::ArrayRef<unsigned> idxs) override {
    return llvm::ExtractValueInst::Create(v, idxs, nextName(), LLVMBB);
  }

  llvm::ReturnInst *createReturn(llvm::Value *v) override {
    return llvm::ReturnInst::Create(Ctx, v, LLVMBB);
  }

  llvm::CallInst *createFShr(llvm::Value *a, llvm::Value *b, llvm::Value *c) override {
    auto *decl =
        llvm::Intrinsic::getDeclaration(LiftedModule, llvm::Intrinsic::fshr, a->getType());
    return llvm::CallInst::Create(decl, {a, b, c}, nextName(), LLVMBB);
  }

  llvm::CallInst *createFShl(llvm::Value *a, llvm::Value *b, llvm::Value *c) override {
    auto *decl =
        llvm::Intrinsic::getDeclaration(LiftedModule, llvm::Intrinsic::fshl, a->getType());
    return llvm::CallInst::Create(decl, {a, b, c}, nextName(), LLVMBB);
  }

  llvm::CallInst *createBitReverse(llvm::Value *v) override {
    auto *decl = llvm::Intrinsic::getDeclaration(LiftedModule, llvm::Intrinsic::bitreverse,
                                           v->getType());
    return llvm::CallInst::Create(decl, {v}, nextName(), LLVMBB);
  }

  llvm::CallInst *createAbs(llvm::Value *v) override {
    auto *decl =
        llvm::Intrinsic::getDeclaration(LiftedModule, llvm::Intrinsic::abs, v->getType());
    return llvm::CallInst::Create(decl, {v, getIntConst(0, 1)}, nextName(), LLVMBB);
  }

  llvm::CallInst *createCtlz(llvm::Value *v) override {
    auto *decl =
        llvm::Intrinsic::getDeclaration(LiftedModule, llvm::Intrinsic::ctlz, v->getType());
    return llvm::CallInst::Create(decl, {v, getIntConst(0, 1)}, nextName(), LLVMBB);
  }

  llvm::CallInst *createBSwap(llvm::Value *v) override {
    auto *decl =
        llvm::Intrinsic::getDeclaration(LiftedModule, llvm::Intrinsic::bswap, v->getType());
    return llvm::CallInst::Create(decl, {v}, nextName(), LLVMBB);
  }

  llvm::CallInst *createVectorReduceAdd(llvm::Value *v) override {
    auto *decl = llvm::Intrinsic::getDeclaration(
        LiftedModule, llvm::Intrinsic::vector_reduce_add, v->getType());
    return llvm::CallInst::Create(decl, {v}, nextName(), LLVMBB);
  }

  llvm::CallInst *createFusedMultiplyAdd(llvm::Value *a, llvm::Value *b, llvm::Value *c) {
    auto *decl =
        llvm::Intrinsic::getDeclaration(LiftedModule, llvm::Intrinsic::fma, a->getType());
    return llvm::CallInst::Create(decl, {a, b, c}, nextName(), LLVMBB);
  }

  llvm::CallInst *createSQRT(llvm::Value *v) {
    auto *decl =
        llvm::Intrinsic::getDeclaration(LiftedModule, llvm::Intrinsic::sqrt, v->getType());
    return llvm::CallInst::Create(decl, {v}, nextName(), LLVMBB);
  }

  llvm::SelectInst *createSelect(llvm::Value *cond, llvm::Value *a, llvm::Value *b) override {
    return llvm::SelectInst::Create(cond, a, b, nextName(), LLVMBB);
  }

  llvm::ICmpInst *createICmp(llvm::ICmpInst::Predicate p, llvm::Value *a, llvm::Value *b) override {
    return new llvm::ICmpInst(*LLVMBB, p, a, b, nextName());
  }

  llvm::FCmpInst *createFCmp(llvm::FCmpInst::Predicate p, llvm::Value *a, llvm::Value *b) override {
    return new llvm::FCmpInst(*LLVMBB, p, a, b, nextName());
  }

  llvm::BinaryOperator *createBinop(llvm::Value *a, llvm::Value *b, llvm::Instruction::BinaryOps op) override {
    return llvm::BinaryOperator::Create(op, a, b, nextName(), LLVMBB);
  }

  llvm::BinaryOperator *createUDiv(llvm::Value *a, llvm::Value *b) override {
    return llvm::BinaryOperator::Create(llvm::Instruction::UDiv, a, b, nextName(), LLVMBB);
  }

  llvm::BinaryOperator *createSDiv(llvm::Value *a, llvm::Value *b) override {
    return llvm::BinaryOperator::Create(llvm::Instruction::SDiv, a, b, nextName(), LLVMBB);
  }

  llvm::BinaryOperator *createMul(llvm::Value *a, llvm::Value *b) override {
    return llvm::BinaryOperator::Create(llvm::Instruction::Mul, a, b, nextName(), LLVMBB);
  }

  llvm::BinaryOperator *createAdd(llvm::Value *a, llvm::Value *b) override {
    return llvm::BinaryOperator::Create(llvm::Instruction::Add, a, b, nextName(), LLVMBB);
  }

  llvm::BinaryOperator *createSub(llvm::Value *a, llvm::Value *b) override {
    return llvm::BinaryOperator::Create(llvm::Instruction::Sub, a, b, nextName(), LLVMBB);
  }

  llvm::Value *createRawLShr(llvm::Value *a, llvm::Value *b) override {
    return llvm::BinaryOperator::Create(llvm::Instruction::LShr, a, b, nextName(), LLVMBB);
  }

  // Create and return a ConstantVector out of the vector of Constant vals
  llvm::Value *getVectorConst(const std::vector<llvm::Constant *> &vals) {
    return llvm::ConstantVector::get(vals);
  }

  // Takes an LLVM Type*, constructs a mask value of this type with
  // mask value = W - 1 where W is the bitwidth of the element type if a vector
  // type or the bitwidth of the type if an integer
  llvm::Value *getMaskByType(llvm::Type *llvm_ty) {
    assert((llvm_ty->isIntegerTy() || llvm_ty->isVectorTy()) &&
           "getMaskByType only handles integer or vector type right now\n");
    llvm::Value *mask_value;

    if (llvm_ty->isIntegerTy()) {
      auto W = llvm_ty->getIntegerBitWidth();
      mask_value = getIntConst(W - 1, W);
    } else if (llvm_ty->isVectorTy()) {
      llvm::VectorType *shift_value_type = ((llvm::VectorType *)llvm_ty);
      auto W_element = shift_value_type->getScalarSizeInBits();
      auto numElements = shift_value_type->getElementCount().getFixedValue();
      std::vector<llvm::Constant *> widths;

      // Push numElements x (W_element-1)'s to the vector widths
      for (unsigned int i = 0; i < numElements; i++) {
        widths.push_back(
            llvm::ConstantInt::get(Ctx, llvm::APInt(W_element, W_element - 1)));
      }

      // Get a ConstantVector of the widths
      mask_value = getVectorConst(widths);
    } else {
      std::cerr << "ERROR: getMaskByType encountered unhandled/unknown type\n";
      exit(-1);
    }

    return mask_value;
  }

  llvm::Value *createMaskedLShr(llvm::Value *a, llvm::Value *b) override {
    assert(a->getType() == b->getType() && "Expected values of same type");

    // Get an LLVM mask for b to get shift value less than bit width of a
    // In LLVM shift >= bitwidth -> poison
    auto mask = getMaskByType(a->getType());
    assert(a->getType() == mask->getType() && "Expected values of same type");

    auto masked =
        llvm::BinaryOperator::Create(llvm::Instruction::And, mask, b, nextName(), LLVMBB);
    return llvm::BinaryOperator::Create(llvm::Instruction::LShr, a, masked, nextName(),
                                  LLVMBB);
  }

  llvm::Value *createRawAShr(llvm::Value *a, llvm::Value *b) override {
    return llvm::BinaryOperator::Create(llvm::Instruction::AShr, a, b, nextName(), LLVMBB);
  }

  llvm::Value *createMaskedAShr(llvm::Value *a, llvm::Value *b) override {
    assert(a->getType() == b->getType() && "Expected values of same type");

    // Get an LLVM mask for b to get shift value less than bit width of a
    // In LLVM shift >= bitwidth -> poison
    auto mask = getMaskByType(a->getType());
    assert(a->getType() == mask->getType() && "Expected values of same type");

    auto masked =
        llvm::BinaryOperator::Create(llvm::Instruction::And, mask, b, nextName(), LLVMBB);
    return llvm::BinaryOperator::Create(llvm::Instruction::AShr, a, masked, nextName(),
                                  LLVMBB);
  }

  llvm::Value *createRawShl(llvm::Value *a, llvm::Value *b) override {
    return llvm::BinaryOperator::Create(llvm::Instruction::Shl, a, b, nextName(), LLVMBB);
  }

  llvm::Value *createMaskedShl(llvm::Value *a, llvm::Value *b) override {
    assert(a->getType() == b->getType() && "Expected values of same type");

    // Get an LLVM mask for b to get shift value less than bit width of a
    // In LLVM shift >= bitwidth -> poison
    auto mask = getMaskByType(a->getType());
    assert(a->getType() == mask->getType() && "Expected values of same type");

    auto masked =
        llvm::BinaryOperator::Create(llvm::Instruction::And, mask, b, nextName(), LLVMBB);
    return llvm::BinaryOperator::Create(llvm::Instruction::Shl, a, masked, nextName(),
                                  LLVMBB);
  }

  llvm::Value *getLowOnes(int ones, int w) override {
    auto zero = getIntConst(0, ones);
    auto one = getIntConst(1, ones);
    auto minusOne = createSub(zero, one);
    return createZExt(minusOne, getIntTy(w));
  }

  llvm::Value *createMSL(llvm::Value *a, int b) override {
    auto v = llvm::BinaryOperator::Create(llvm::Instruction::Shl, a,
                                    getIntConst(b, getBitWidth(a)), nextName(),
                                    LLVMBB);
    auto ones = getLowOnes(b, getBitWidth(a));
    return createOr(v, ones);
  }

  llvm::BinaryOperator *createAnd(llvm::Value *a, llvm::Value *b) override {
    return llvm::BinaryOperator::Create(llvm::Instruction::And, a, b, nextName(), LLVMBB);
  }

  llvm::BinaryOperator *createOr(llvm::Value *a, llvm::Value *b) override {
    return llvm::BinaryOperator::Create(llvm::Instruction::Or, a, b, nextName(), LLVMBB);
  }

  llvm::BinaryOperator *createXor(llvm::Value *a, llvm::Value *b) override {
    return llvm::BinaryOperator::Create(llvm::Instruction::Xor, a, b, nextName(), LLVMBB);
  }

  llvm::BinaryOperator *createNot(llvm::Value *a) override {
    auto *ty = a->getType();
    auto Zero = llvm::ConstantInt::get(ty, 0);
    auto One = llvm::ConstantInt::get(ty, 1);
    auto NegOne =
        llvm::BinaryOperator::Create(llvm::Instruction::Sub, Zero, One, nextName(), LLVMBB);
    return llvm::BinaryOperator::Create(llvm::Instruction::Xor, a, NegOne, nextName(),
                                  LLVMBB);
  }

  llvm::FreezeInst *createFreeze(llvm::Value *v) override {
    return new llvm::FreezeInst(v, nextName(), LLVMBB);
  }

  llvm::Value *createTrunc(llvm::Value *v, llvm::Type *t) override {
    if (v->getType() == t)
      return v;
    return llvm::CastInst::Create(llvm::Instruction::Trunc, v, t, nextName(), LLVMBB);
  }

  llvm::CastInst *createSExt(llvm::Value *v, llvm::Type *t) override {
    return llvm::CastInst::Create(llvm::Instruction::SExt, v, t, nextName(), LLVMBB);
  }

  llvm::CastInst *createZExt(llvm::Value *v, llvm::Type *t) override {
    return llvm::CastInst::Create(llvm::Instruction::ZExt, v, t, nextName(), LLVMBB);
  }

  llvm::CastInst *createFPToUI(llvm::Value *v, llvm::Type *t) {
    return llvm::CastInst::Create(llvm::Instruction::FPToUI, v, t, nextName(), LLVMBB);
  }

  llvm::CastInst *createFPToSI(llvm::Value *v, llvm::Type *t) {
    return llvm::CastInst::Create(llvm::Instruction::FPToSI, v, t, nextName(), LLVMBB);
  }

  llvm::CastInst *createUIToFP(llvm::Value *v, llvm::Type *t) override {
    return llvm::CastInst::Create(llvm::Instruction::UIToFP, v, t, nextName(), LLVMBB);
  }

  llvm::CastInst *createSIToFP(llvm::Value *v, llvm::Type *t) override {
    return llvm::CastInst::Create(llvm::Instruction::SIToFP, v, t, nextName(), LLVMBB);
  }

  llvm::CastInst *createFPTrunc(llvm::Value *v, llvm::Type *t) {
    return llvm::CastInst::Create(llvm::Instruction::FPTrunc, v, t, nextName(), LLVMBB);
  }

  llvm::CastInst *createFPExt(llvm::Value *v, llvm::Type *t) {
    return llvm::CastInst::Create(llvm::Instruction::FPExt, v, t, nextName(), LLVMBB);
  }

  llvm::CastInst *createBitCast(llvm::Value *v, llvm::Type *t) override {
    return llvm::CastInst::Create(llvm::Instruction::BitCast, v, t, nextName(), LLVMBB);
  }

  llvm::CastInst *createCast(llvm::Value *v, llvm::Type *t, llvm::Instruction::CastOps op) override {
    return llvm::CastInst::Create(op, v, t, nextName(), LLVMBB);
  }


  static unsigned int getBitWidth(llvm::Type *ty) {
    if (auto vTy = llvm::dyn_cast<llvm::VectorType>(ty)) {
      return vTy->getScalarSizeInBits() *
             vTy->getElementCount().getFixedValue();
    } else if (ty->isIntegerTy()) {
      return ty->getIntegerBitWidth();
    } else if (ty->isHalfTy()) {
      return 16;
    } else if (ty->isFloatTy()) {
      return 32;
    } else if (ty->isDoubleTy()) {
      return 64;
    } else if (ty->isPointerTy()) {
      return 64;
    } else {
      ty->dump();
      assert(false && "Unhandled type");
    }
  }

  static unsigned int getBitWidth(llvm::Value *V) {
    return getBitWidth(V->getType());
  }

  // // Returns bitWidth corresponding the registers
  // unsigned getRegSize(unsigned Reg) {
  //   if (Reg == AArch64::N || Reg == AArch64::Z || Reg == AArch64::C ||
  //       Reg == AArch64::V)
  //     return 1;
  //   if (Reg >= AArch64::B0 && Reg <= AArch64::B31)
  //     return 8;
  //   if (Reg >= AArch64::H0 && Reg <= AArch64::H31)
  //     return 16;
  //   if ((Reg >= AArch64::W0 && Reg <= AArch64::W30) ||
  //       (Reg >= AArch64::S0 && Reg <= AArch64::S31) || Reg == AArch64::WZR ||
  //       Reg == AArch64::WSP)
  //     return 32;
  //   if ((Reg >= AArch64::X0 && Reg <= AArch64::X28) ||
  //       (Reg >= AArch64::D0 && Reg <= AArch64::D31) || Reg == AArch64::XZR ||
  //       Reg == AArch64::SP || Reg == AArch64::FP || Reg == AArch64::LR)
  //     return 64;
  //   if (Reg >= AArch64::Q0 && Reg <= AArch64::Q31)
  //     return 128;
  //   assert(false && "unhandled register");
  // }

  // Maps ARM registers to backing registers
  // unsigned mapRegToBackingReg(unsigned Reg) {
  //   if (Reg == AArch64::WZR)
  //     return AArch64::XZR;
  //   else if (Reg == AArch64::W29)
  //     return AArch64::FP;
  //   else if (Reg == AArch64::W30)
  //     return AArch64::LR;
  //   else if (Reg == AArch64::WSP)
  //     return AArch64::SP;
  //   else if (Reg >= AArch64::W0 && Reg <= AArch64::W28)
  //     return Reg - AArch64::W0 + AArch64::X0;
  //   else if (Reg >= AArch64::X0 && Reg <= AArch64::X28)
  //     return Reg - AArch64::X0 + AArch64::X0;
  //   // Dealias rules for NEON SIMD/floating-point registers
  //   // https://developer.arm.com/documentation/den0024/a/AArch64-Floating-point-and-NEON/NEON-and-Floating-Point-architecture/Floating-point
  //   else if (Reg >= AArch64::B0 && Reg <= AArch64::B31)
  //     return Reg - AArch64::B0 + AArch64::Q0;
  //   else if (Reg >= AArch64::H0 && Reg <= AArch64::H31)
  //     return Reg - AArch64::H0 + AArch64::Q0;
  //   else if (Reg >= AArch64::S0 && Reg <= AArch64::S31)
  //     return Reg - AArch64::S0 + AArch64::Q0;
  //   else if (Reg >= AArch64::D0 && Reg <= AArch64::D31)
  //     return Reg - AArch64::D0 + AArch64::Q0;
  //   assert(RegFile[Reg] &&
  //          "ERROR: Cannot have a register without a backing store"
  //          " register corresponding it.");
  //   return Reg;
  // }

  // return pointer to the backing store for a register, doing the
  // necessary de-aliasing
  // llvm::Value *dealiasReg(unsigned Reg) {
  //   auto RegAddr = RegFile[mapRegToBackingReg(Reg)];
  //   assert(RegAddr);
  //   return RegAddr;
  // }
  //
  // // always does a full-width read
  // llvm::Value *readFromReg(unsigned Reg) {
  //   auto RegAddr = dealiasReg(Reg);
  //   return createLoad(getIntTy(getRegSize(mapRegToBackingReg(Reg))), RegAddr);
  // }
  //
  // llvm::Value *readPtrFromReg(unsigned Reg) {
  //   auto RegAddr = dealiasReg(Reg);
  //   return createLoad(Pointerllvm::Type::get(Ctx, 0), RegAddr);
  // }

  // void updateOutputReg(llvm::Value *V, bool SExt = false) override {
  //   auto destReg = CurInst->getOperand(0).getReg();
  //   updateReg(V, destReg, SExt);
  // }

  void storeToMemoryValOffset(llvm::Value *base, llvm::Value *offset, u_int64_t size,
                              llvm::Value *val) override {
    // Create a GEP instruction based on a byte addressing basis (8 bits)
    // returning pointer to base + offset
    assert(base);
    auto ptr = createGEP(getIntTy(8), base, {offset}, "");

    // Store llvm::Value val in the pointer returned by the GEP instruction
    createStore(val, ptr);
  }

public:
  arm2llvm(llvm::Module *LiftedModule, llvm::Function &srcFn)
      : LiftedModule(LiftedModule) { };

};

