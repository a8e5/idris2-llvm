#include "llvm/Pass.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/raw_ostream.h"

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

using namespace llvm;

// The next function is a part of Julia. License is MIT: https://julialang.org/license
template<typename TIterator>
static void replaceInstruction(
    Instruction *oldInstruction,
    Value *newInstruction,
    TIterator &it)
{
    if (newInstruction != oldInstruction) {
        oldInstruction->replaceAllUsesWith(newInstruction);
        it = oldInstruction->eraseFromParent();
    }
    else {
        ++it;
    }
}


namespace {
struct LowerRapidIntrinsics : public PassInfoMixin<LowerRapidIntrinsics> {
  PreservedAnalyses run(Function &F, FunctionAnalysisManager &FAM) {
    runOnFunction(F);
    return PreservedAnalyses::none();
  }

  Value *lowerBoxint(CallInst *target, Function &F) {
    assert(target->getNumArgOperands() == 1);
    auto arg0 = target->getArgOperand(0);

    auto &ctx = target->getContext();

    IRBuilder<> builder(ctx);
    builder.SetInsertPoint(target);

    auto shifted = builder.CreateShl(arg0, ConstantInt::get(arg0->getType(), 1));
    auto tagged = builder.CreateOr(shifted, ConstantInt::get(arg0->getType(), 1));
    auto castToPtr = builder.CreateIntToPtr(tagged, PointerType::get(target->getType()->getPointerElementType(), 2));
    auto castToAS1 = builder.CreateAddrSpaceCast(castToPtr, target->getType());
    return castToAS1;
  }


  Value *lowerUnboxint(CallInst *target, Function &F) {
    assert(target->getNumArgOperands() == 1);
    auto arg0 = target->getArgOperand(0);

    auto &ctx = target->getContext();

    IRBuilder<> builder(ctx);
    builder.SetInsertPoint(target);

    auto castToAS2 = builder.CreateAddrSpaceCast(arg0, PointerType::get(arg0->getType()->getPointerElementType(), 2));
    auto castToInt = builder.CreatePtrToInt(castToAS2, Type::getInt64Ty(ctx));
    auto shifted = builder.CreateAShr(castToInt, ConstantInt::get(castToInt->getType(), 1));
    return shifted;
  }

  Value *lowerIsDirect(CallInst *target, Function &F) {
    assert(target->getNumArgOperands() == 1);
    auto arg0 = target->getArgOperand(0);

    auto &ctx = target->getContext();

    IRBuilder<> builder(ctx);
    builder.SetInsertPoint(target);

    auto castToAS2 = builder.CreateAddrSpaceCast(arg0, PointerType::get(arg0->getType()->getPointerElementType(), 2));
    auto castToInt = builder.CreatePtrToInt(castToAS2, Type::getInt64Ty(ctx));
    auto masked = builder.CreateAnd(castToInt, ConstantInt::get(castToInt->getType(), 1));
    auto cmpEq1 = builder.CreateICmpEQ(masked, ConstantInt::get(castToInt->getType(), 1));
    return cmpEq1;
  }

  bool runOnFunction(Function &F) {
    auto &M = *F.getParent();
    Function *rapid_boxint = M.getFunction("llvm.rapid.boxint");
    Function *rapid_unboxint = M.getFunction("llvm.rapid.unboxint");
    Function *rapid_isdirect = M.getFunction("llvm.rapid.isdirect");

    for (BasicBlock &BB : F) {
      for (auto it = BB.begin(); it != BB.end();) {
        auto *CI = dyn_cast<CallInst>(&*it);
        if (!CI) {
          ++it;
          continue;
        }

        auto callee = CI->getCalledFunction();
        if (!callee) {
          ++it;
          continue;
        }

        if (callee == rapid_boxint) {
          //errs().write_escaped("found rapid.boxint") << '\n';
          replaceInstruction(CI, lowerBoxint(CI, F), it);
        } else if (callee == rapid_unboxint) {
          //errs().write_escaped("found rapid.unboxint") << '\n';
          replaceInstruction(CI, lowerUnboxint(CI, F), it);
        } else if (callee == rapid_isdirect) {
          //errs().write_escaped("found rapid.isdirect") << '\n';
          replaceInstruction(CI, lowerIsDirect(CI, F), it);
        } else {
          ++it;
        }
      }
    }

    return false;
  }
};

struct LowerRapidIntrinsicsLegacy : public FunctionPass {
  static char ID;
  LowerRapidIntrinsics ModernPass;
  LowerRapidIntrinsicsLegacy() : FunctionPass(ID) {}

  bool doInitialization(Module &M) override {
    return false;
  }

  bool runOnFunction(Function &F) override {
    return ModernPass.runOnFunction(F);
  }
};
}

// Register pass with the legacy pass manager
char LowerRapidIntrinsicsLegacy::ID = 0;
static RegisterPass<LowerRapidIntrinsicsLegacy> X("rapid-lower", "Lower Rapid Intrinsics",
                             false /* Only looks at CFG */,
                             false /* Analysis Pass */);

// Register pass with the new pass manager
extern "C" ::llvm::PassPluginLibraryInfo LLVM_ATTRIBUTE_WEAK
llvmGetPassPluginInfo() {
  return {
    LLVM_PLUGIN_API_VERSION, "LowerRapidIntrinsics", "0.0.1",
    [](PassBuilder &PB) {
      PB.registerPipelineParsingCallback(
        [](StringRef Name, FunctionPassManager &FPM,
        ArrayRef<PassBuilder::PipelineElement>) {
          if(Name == "rapid-lower"){
            FPM.addPass(LowerRapidIntrinsics());
            return true;
          }
          return false;
        }
      );
    }
  };
}
