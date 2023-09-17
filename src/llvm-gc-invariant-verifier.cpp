// This file is a part of Julia. License is MIT: https://julialang.org/license

// This LLVM pass verifies invariants required for correct GC root placement.
// See the devdocs for a description of these invariants.

#include "llvm-version.h"
#include "passes.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

#include <llvm/ADT/BitVector.h>
#include <llvm/ADT/PostOrderIterator.h>
#include <llvm/Analysis/CFG.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/InstVisitor.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Pass.h>
#include <llvm/Support/Debug.h>

#include "llvm-codegen-shared.h"
#include "julia.h"

#define DEBUG_TYPE "verify_gc_invariants"
#undef DEBUG

using namespace llvm;

struct GCInvariantVerifier : public InstVisitor<GCInvariantVerifier> {
    bool Broken = false;
    bool Strong;
    GCInvariantVerifier(bool Strong = false) : Strong(Strong) {}

private:
    void Check(bool Cond, const char *message, Value *Val) {
        if (!Cond) {
            dbgs() << message << "\n\t" << *Val << "\n";
            Broken = true;
        }
    }

public:
    void visitAddrSpaceCastInst(AddrSpaceCastInst &I);
    void visitLoadInst(LoadInst &LI);
    void visitStoreInst(StoreInst &SI);
    void visitAtomicCmpXchgInst(AtomicCmpXchgInst &SI);
    void visitAtomicRMWInst(AtomicRMWInst &SI);
    void visitReturnInst(ReturnInst &RI);
    void visitGetElementPtrInst(GetElementPtrInst &GEP);
    void visitIntToPtrInst(IntToPtrInst &IPI);
    void visitPtrToIntInst(PtrToIntInst &PII);
    void visitCallInst(CallInst &CI);

    void checkStoreInst(Type *VTy, unsigned AS, Value &SI);
};

void GCInvariantVerifier::visitAddrSpaceCastInst(AddrSpaceCastInst &I) {
    unsigned FromAS = cast<PointerType>(I.getSrcTy())->getAddressSpace();
    unsigned ToAS = cast<PointerType>(I.getDestTy())->getAddressSpace();
    if (FromAS == 0)
        return;
    Check(ToAS != AddressSpace::Loaded && FromAS != AddressSpace::Loaded,
          "Illegal address space cast involving loaded ptr", &I);
    Check(FromAS != AddressSpace::Tracked ||
          ToAS   == AddressSpace::CalleeRooted ||
          ToAS   == AddressSpace::Derived,
          "Illegal address space cast from tracked ptr", &I);
    Check(FromAS != AddressSpace::CalleeRooted &&
          FromAS != AddressSpace::Derived,
          "Illegal address space cast from decayed ptr", &I);
}

void GCInvariantVerifier::checkStoreInst(Type *VTy, unsigned AS, Value &SI) {
    if (VTy->isPointerTy()) {
        /* We currently don't obey this for arguments. That's ok - they're
           externally rooted. */
        unsigned AS = cast<PointerType>(VTy)->getAddressSpace();
        Check(AS != AddressSpace::CalleeRooted &&
              AS != AddressSpace::Derived,
              "Illegal store of decayed value", &SI);
    }
    Check(AS != AddressSpace::CalleeRooted,
          "Illegal store to callee rooted value", &SI);
}

void GCInvariantVerifier::visitStoreInst(StoreInst &SI) {
    Type *VTy = SI.getValueOperand()->getType();
    checkStoreInst(VTy, SI.getPointerAddressSpace(), SI);
}

void GCInvariantVerifier::visitAtomicRMWInst(AtomicRMWInst &SI) {
    Type *VTy = SI.getValOperand()->getType();
    checkStoreInst(VTy, SI.getPointerAddressSpace(), SI);
}

void GCInvariantVerifier::visitAtomicCmpXchgInst(AtomicCmpXchgInst &SI) {
    Type *VTy = SI.getNewValOperand()->getType();
    checkStoreInst(VTy, SI.getPointerAddressSpace(), SI);
}

void GCInvariantVerifier::visitLoadInst(LoadInst &LI) {
    Type *Ty = LI.getType();
    if (Ty->isPointerTy()) {
        unsigned AS = cast<PointerType>(Ty)->getAddressSpace();
        Check(AS != AddressSpace::CalleeRooted &&
              AS != AddressSpace::Derived,
              "Illegal load of gc relevant value", &LI);
    }
    Ty = LI.getPointerOperand()->getType();
    if (Ty->isPointerTy()) {
        unsigned AS = cast<PointerType>(Ty)->getAddressSpace();
        Check(AS != AddressSpace::CalleeRooted,
              "Illegal load of callee rooted value", &LI);
    }
}

static bool isSpecialAS(unsigned AS) {
    return AddressSpace::FirstSpecial <= AS && AS <= AddressSpace::LastSpecial;
}

void GCInvariantVerifier::visitReturnInst(ReturnInst &RI) {
    if (!RI.getReturnValue())
        return;
    Type *RTy = RI.getReturnValue()->getType();
    if (!RTy->isPointerTy())
        return;
    unsigned AS = cast<PointerType>(RTy)->getAddressSpace();
    Check(!isSpecialAS(AS) || AS == AddressSpace::Tracked,
          "Only gc tracked values may be directly returned", &RI);
}

void GCInvariantVerifier::visitGetElementPtrInst(GetElementPtrInst &GEP) {
    Type *Ty = GEP.getType();
    if (!Ty->isPointerTy())
        return;
    unsigned AS = cast<PointerType>(Ty)->getAddressSpace();
    if (!isSpecialAS(AS))
        return;
    /* We're actually ok with GEPs here, as long as they don't feed into any
       uses. Upstream is currently still debating whether CAST(GEP) == GEP(CAST).
       In the frontend, we always perform CAST(GEP), so while we can enforce
       this invariant when we run directly after the frontend (Strong == 1),
       the optimizer will introduce the other form. Thus, we need to allow it
       while upstream hasn't decided whether the optimizer is allowed to
       introduce these.
       */
    if (Strong) {
        Check(AS != AddressSpace::Tracked,
             "GC tracked values may not appear in GEP expressions."
             " You may have to decay the value first", &GEP);
    }
}

void GCInvariantVerifier::visitCallInst(CallInst &CI) {
    Function *Callee = CI.getCalledFunction();
    if (Callee && (Callee->getName() == "julia.call" ||
                   Callee->getName() == "julia.call2")) {
        bool First = true;
        for (Value *Arg : CI.args()) {
            Type *Ty = Arg->getType();
            Check(Ty->isPointerTy() && cast<PointerType>(Ty)->getAddressSpace() == (First ? 0 : AddressSpace::Tracked),
                "Invalid derived pointer in jlcall", &CI);
            First = false;
        }
    }
}

/* These next two are caught by the regular verifier on LLVM 5.0+, but we
   may want to run this on earlier LLVM versions. */
void GCInvariantVerifier::visitIntToPtrInst(IntToPtrInst &IPI) {
    Check(!isSpecialAS(IPI.getAddressSpace()),
          "Illegal inttoptr", &IPI);
}

void GCInvariantVerifier::visitPtrToIntInst(PtrToIntInst &PII) {
    Check(!isSpecialAS(PII.getPointerAddressSpace()),
          "Illegal inttoptr", &PII);
}

PreservedAnalyses GCInvariantVerifierPass::run(Function &F, FunctionAnalysisManager &AM) {
    GCInvariantVerifier GIV(Strong);
    GIV.visit(F);
    if (GIV.Broken) {
        abort();
    }
    return PreservedAnalyses::all();
}
