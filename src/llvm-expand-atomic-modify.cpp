// This file is a part of Julia. License is MIT: https://julialang.org/license

// TODO: move this feature into AtomicExpandImpl

#include "llvm-version.h"
#include "passes.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

#include <llvm/Analysis/InstSimplifyFolder.h>
#include <llvm/CodeGen/AtomicExpandUtils.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Operator.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Pass.h>
#include <llvm/Support/Debug.h>
#include <llvm/Transforms/Utils/LowerAtomic.h>

#include "julia.h"
#include "julia_assert.h"

#define DEBUG_TYPE "expand-atomic-modify"
#undef DEBUG

using namespace llvm;

// This pass takes fake call instructions that look like this that were emitted by the front end:
//   oldval = atomicrmw xchg ptr, val ordering syncscope
//   newval = call atomicmodify.iN(op, oldval)
// Then rewrite that to
//   oldval = atomicrmw op ptr, val ordering syncscope
//   newval = op oldval, val
// Or to an equivalent RMWCmpXchgLoop if `op` isn't valid for atomicrmw


// from AtomicExpandImpl, with modification of failure order
static void createCmpXchgInstFun(IRBuilderBase &Builder, Value *Addr,
                                 Value *Loaded, Value *NewVal, Align AddrAlign,
                                 AtomicOrdering MemOpOrder, SyncScope::ID SSID,
                                 Value *&Success, Value *&NewLoaded) {
  Type *OrigTy = NewVal->getType();

  // This code can go away when cmpxchg supports FP types.
  assert(!OrigTy->isPointerTy());
  bool NeedBitcast = OrigTy->isFloatingPointTy();
  if (NeedBitcast) {
    IntegerType *IntTy = Builder.getIntNTy(OrigTy->getPrimitiveSizeInBits());
    NewVal = Builder.CreateBitCast(NewVal, IntTy);
    Loaded = Builder.CreateBitCast(Loaded, IntTy);
  }

  Value *Pair = Builder.CreateAtomicCmpXchg(
      Addr, Loaded, NewVal, AddrAlign, MemOpOrder,
      AtomicOrdering::Monotonic, // AtomicCmpXchgInst::getStrongestFailureOrdering(MemOpOrder),
      SSID);
  Success = Builder.CreateExtractValue(Pair, 1, "success");
  NewLoaded = Builder.CreateExtractValue(Pair, 0, "newloaded");

  if (NeedBitcast)
    NewLoaded = Builder.CreateBitCast(NewLoaded, OrigTy);
}

// from AtomicExpandImpl, with modification of values returned
std::pair<Value *, Value *> insertRMWCmpXchgLoop(
    IRBuilderBase &Builder, Type *ResultTy, Value *Addr, Align AddrAlign,
    AtomicOrdering MemOpOrder, SyncScope::ID SSID,
    function_ref<Value *(IRBuilderBase &, Value *)> PerformOp,
    CreateCmpXchgInstFun CreateCmpXchg) {
  LLVMContext &Ctx = Builder.getContext();
  BasicBlock *BB = Builder.GetInsertBlock();
  Function *F = BB->getParent();

  // Given: atomicrmw some_op iN* %addr, iN %incr ordering
  //
  // The standard expansion we produce is:
  //     [...]
  //     %init_loaded = load atomic iN* %addr
  //     br label %loop
  // loop:
  //     %loaded = phi iN [ %init_loaded, %entry ], [ %new_loaded, %loop ]
  //     %new = some_op iN %loaded, %incr
  //     %pair = cmpxchg iN* %addr, iN %loaded, iN %new
  //     %new_loaded = extractvalue { iN, i1 } %pair, 0
  //     %success = extractvalue { iN, i1 } %pair, 1
  //     br i1 %success, label %atomicrmw.end, label %loop
  // atomicrmw.end:
  //     [...]
  BasicBlock *ExitBB =
      BB->splitBasicBlock(Builder.GetInsertPoint(), "atomicrmw.end");
  BasicBlock *LoopBB = BasicBlock::Create(Ctx, "atomicrmw.start", F, ExitBB);

  // The split call above "helpfully" added a branch at the end of BB (to the
  // wrong place), but we want a load. It's easiest to just remove
  // the branch entirely.
  std::prev(BB->end())->eraseFromParent();
  Builder.SetInsertPoint(BB);
  LoadInst *InitLoaded = Builder.CreateAlignedLoad(ResultTy, Addr, AddrAlign);
  Builder.CreateBr(LoopBB);

  // Start the main loop block now that we've taken care of the preliminaries.
  Builder.SetInsertPoint(LoopBB);
  PHINode *Loaded = Builder.CreatePHI(ResultTy, 2, "loaded");
  Loaded->addIncoming(InitLoaded, BB);

  Value *NewVal = PerformOp(Builder, Loaded);

  Value *NewLoaded = nullptr;
  Value *Success = nullptr;

  CreateCmpXchg(Builder, Addr, Loaded, NewVal, AddrAlign,
                MemOpOrder == AtomicOrdering::Unordered
                    ? AtomicOrdering::Monotonic
                    : MemOpOrder,
                SSID, Success, NewLoaded);
  assert(Success && NewLoaded);

  Loaded->addIncoming(NewLoaded, LoopBB);

  Builder.CreateCondBr(Success, ExitBB, LoopBB);

  Builder.SetInsertPoint(ExitBB, ExitBB->begin());
  return {NewLoaded, NewVal};
}

// from AtomicExpandImpl
struct ReplacementIRBuilder : IRBuilder<InstSimplifyFolder> {
  // Preserves the DebugLoc from I, and preserves still valid metadata.
  explicit ReplacementIRBuilder(Instruction *I, const DataLayout &DL)
      : IRBuilder(I->getContext(), DL) {
    SetInsertPoint(I);
    this->CollectMetadataToCopy(I, {LLVMContext::MD_pcsections});
  }
};

static AtomicRMWInst::BinOp patternMatchAtomicRMWOp(Value *&Op, bool &swiftcall)
{
    bool recursive = true;
restart:
    Function *F = dyn_cast<Function>(Op);
    if (!F || !F->hasExactDefinition() || F->isIntrinsic())
        return AtomicRMWInst::BAD_BINOP;
    BasicBlock &BB = F->getEntryBlock();
    ReturnInst *Ret = dyn_cast_or_null<ReturnInst>(BB.getTerminator());
    // TODO: be able to ignore any simple transforms to Ret by hoisting them into the caller
    if (!Ret)
        return AtomicRMWInst::BAD_BINOP;
    // Now we know Op is a Function with exactly one basic block
    // Now examine the instruction list
    Argument *Old = F->getArg(swiftcall ? 1 : 0);
    Argument *Val = F->getArg(swiftcall ? 2 : 1);
    // TODO: be able to ignore any simple transforms to Val by hoisting them into the caller
    Instruction *Inst = BB.getFirstNonPHIOrDbgOrLifetime();
    if (Inst == Ret) {
        // zero instruction functions
        if (Ret->getReturnValue() == Val)
            return AtomicRMWInst::Xchg;
        return AtomicRMWInst::BAD_BINOP; // TODO: actually this is a constant store or load, so we could handle this too
    }
    Instruction *Next = Inst->getNextNonDebugInstruction();
    if (Next == Ret) {
        // one instruction functions
        if (Ret->getReturnValue() == Inst) {
            if (auto BinOp = dyn_cast<BinaryOperator>(Inst)) {
                if ((BinOp->getOperand(0) == Old && BinOp->getOperand(1) == Val) ||
                    (BinOp->isCommutative() && BinOp->getOperand(1) == Old && BinOp->getOperand(0) == Val)) {
                    switch (BinOp->getOpcode()) {
                      case Instruction::Add:
                        return AtomicRMWInst::Add;
                      case Instruction::Sub:
                        return AtomicRMWInst::Sub;
                      case Instruction::And:
                        return AtomicRMWInst::And;
                      case Instruction::Or:
                        return AtomicRMWInst::Or;
                      case Instruction::Xor:
                        return AtomicRMWInst::Xor;
                      case Instruction::FAdd:
                        return AtomicRMWInst::FAdd;
                      case Instruction::FSub:
                        return AtomicRMWInst::FSub;
                      default:
                        break;
                    }
                }
            }
            else if (auto Intr = dyn_cast<IntrinsicInst>(Inst)) {
              if (Intr->arg_size() == 2) {
                if ((Intr->getOperand(0) == Old && Intr->getOperand(1) == Val) ||
                    (Intr->isCommutative() && Intr->getOperand(1) == Old && Intr->getOperand(0) == Val)) {
                  switch (Intr->getIntrinsicID()) {
                    case Intrinsic::minnum:
                      return AtomicRMWInst::FMin;
                    case Intrinsic::maxnum:
                      return AtomicRMWInst::FMax;
                    default:
                       return AtomicRMWInst::BAD_BINOP;
                  }
                }
              }
            }
            else if (auto Intr = dyn_cast<CallInst>(Inst)) {
              bool intr_swiftcall = Intr->getCallingConv() == CallingConv::Swift;
              if (recursive && Intr->arg_size() == (intr_swiftcall ? 3 : 2) && Intr->getCalledFunction()) {
                if ((!intr_swiftcall && Intr->getOperand(0) == Old && Intr->getOperand(1) == Val) ||
                    (swiftcall && intr_swiftcall && Intr->getOperand(0) == F->getArg(0) && Intr->getOperand(1) == Old && Intr->getOperand(2) == Val)) {
                  // TODO: fixup debuginfo for this effective inlining, if necessary
                  Op = Intr->getCalledOperand();
                  swiftcall = intr_swiftcall;
                  recursive = false;
                  goto restart;
                }
              }
            }
            // TODO: does this need to deal with F->hasFnAttribute(Attribute::StrictFP)?
            // TODO: does Fneg and Neg have expansions?
            // TODO: be able to hoist and ignore bitcasts (particularly i64 to f64)
        }
        return AtomicRMWInst::BAD_BINOP;
    }
    // TODO: handle longer sequences (Nand, Min, Max, UMax, UMin, UIncWrap, UDecWrap, and target-specific ones for CUDA)
    return AtomicRMWInst::BAD_BINOP;
}

void expandAtomicModifyToCmpXchg(CallInst *Modify,
                                 CreateCmpXchgInstFun CreateCmpXchg) {
  bool swiftcall = Modify->getCallingConv() == CallingConv::Swift;
  Value *Op = Modify->getOperand(swiftcall ? 1 : 0);
  AtomicRMWInst *AI = cast<AtomicRMWInst>(Modify->getOperand(swiftcall ? 2 : 1));
  assert(AI->getOperation() == AtomicRMWInst::Xchg);
  assert(AI->getType() == Modify->getType());

  ReplacementIRBuilder Builder(Modify, Modify->getModule()->getDataLayout());
  Builder.setIsFPConstrained(
      AI->getFunction()->hasFnAttribute(Attribute::StrictFP));

  auto BinOp = patternMatchAtomicRMWOp(Op, swiftcall);
  if (BinOp != AtomicRMWInst::BAD_BINOP) {
      AI->setOperation(BinOp);
      if (!Modify->use_empty()) {
          auto NewVal = buildAtomicRMWValue(BinOp, Builder, AI, AI->getValOperand());
          if (NewVal != AI->getValOperand())
            NewVal->takeName(Modify);
          Modify->replaceAllUsesWith(NewVal);
      }
      Modify->eraseFromParent();
      return;
  }

  // FIXME: If FP exceptions are observable, we should force them off for the
  // loop for the FP atomics.
  auto OldNew = insertRMWCmpXchgLoop(
      Builder, AI->getType(), AI->getPointerOperand(), AI->getAlign(),
      AI->getOrdering(), AI->getSyncScopeID(),
      [&](IRBuilderBase &Builder, Value *Loaded) {
        CallInst *NewVal;
        if (swiftcall) {
          FunctionType *FT = FunctionType::get(AI->getType(), {Modify->getOperand(0)->getType(), AI->getType(), AI->getType()}, false);
          NewVal = Builder.CreateCall(FT, Op, {Modify->getOperand(0), Loaded, AI->getValOperand()});
          NewVal->setCallingConv(CallingConv::Swift);
        }
        else {
          FunctionType *FT = FunctionType::get(AI->getType(), {AI->getType(), AI->getType()}, false);
          NewVal = Builder.CreateCall(FT, Op, {Loaded, AI->getValOperand()});
        }
        NewVal->takeName(Modify);
        return NewVal;
      },
      CreateCmpXchg);

  if (isa<Instruction>(OldNew.first))
    OldNew.first->takeName(AI);
  AI->replaceAllUsesWith(OldNew.first);
  AI->eraseFromParent();
  Modify->replaceAllUsesWith(OldNew.second);
  Modify->eraseFromParent();
}

static bool expandAtomicModify(Function &F) {
  SmallVector<Instruction *, 0> AtomicInsts;

  // Changing control-flow while iterating through it is a bad idea, so gather a
  // list of all atomic instructions before we start.
  for (Instruction &I : instructions(F))
    if (auto CI = dyn_cast<CallInst>(&I)) {
      auto callee = CI->getCalledFunction();
      if (callee && callee->getName().startswith("julia.atomicmodify."))
        AtomicInsts.push_back(&I);
      if (callee && callee->getName().startswith("julia.atomicmodify_swiftcc."))
        AtomicInsts.push_back(&I);
    }

  bool MadeChange = false;
  for (auto *I : AtomicInsts) {
    MadeChange = true;
    expandAtomicModifyToCmpXchg(cast<CallInst>(I), createCmpXchgInstFun);
  }
  return MadeChange;
}

PreservedAnalyses ExpandAtomicModifyPass::run(Function &F, FunctionAnalysisManager &AM)
{
    if (expandAtomicModify(F)) {
        return PreservedAnalyses::none();
    }
    return PreservedAnalyses::all();
}
