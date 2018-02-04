//===-- GBZ80TargetMachine.cpp - Define TargetMachine for GBZ80 ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the GBZ80 specific subclass of TargetMachine.
//
//===----------------------------------------------------------------------===//

#include "GBZ80TargetMachine.h"

#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/TargetRegistry.h"

#include "GBZ80.h"
#include "GBZ80TargetObjectFile.h"
#include "MCTargetDesc/GBZ80MCTargetDesc.h"

namespace llvm {

static const char *GBZ80DataLayout = "e-p:16:8-i8:8-i16:8-i32:8-i64:8-f32:8-f64:8-n8-a:8";

/// Processes a CPU name.
static StringRef getCPU(StringRef CPU) {
  if (CPU.empty() || CPU == "generic") {
    return "gb";
  }

  return CPU;
}

static Reloc::Model getEffectiveRelocModel(Optional<Reloc::Model> RM) {
  return RM.hasValue() ? *RM : Reloc::Static;
}

GBZ80TargetMachine::GBZ80TargetMachine(const Target &T, const Triple &TT,
                                   StringRef CPU, StringRef FS,
                                   const TargetOptions &Options,
                                   Optional<Reloc::Model> RM, CodeModel::Model CM,
                                   CodeGenOpt::Level OL)
    : LLVMTargetMachine(
          T, GBZ80DataLayout, TT,
          getCPU(CPU), FS, Options, getEffectiveRelocModel(RM), CM, OL),
      SubTarget(TT, getCPU(CPU), FS, *this) {
  this->TLOF = make_unique<GBZ80TargetObjectFile>();
  initAsmInfo();
}

namespace {
/// GBZ80 Code Generator Pass Configuration Options.
class GBZ80PassConfig : public TargetPassConfig {
public:
  GBZ80PassConfig(GBZ80TargetMachine &TM, PassManagerBase &PM)
      : TargetPassConfig(TM, PM) {}

  GBZ80TargetMachine &getGBZ80TargetMachine() const {
    return getTM<GBZ80TargetMachine>();
  }

  bool addInstSelector() override;
  void addPreSched2() override;
  void addPreEmitPass() override;
  void addPreRegAlloc() override;
};
} // namespace

TargetPassConfig *GBZ80TargetMachine::createPassConfig(PassManagerBase &PM) {
  return new GBZ80PassConfig(*this, PM);
}

extern "C" void LLVMInitializeGBZ80Target() {
  // Register the target.
  RegisterTargetMachine<GBZ80TargetMachine> X(getTheGBZ80Target());

  auto &PR = *PassRegistry::getPassRegistry();
  initializeGBZ80ExpandPseudoPass(PR);
  initializeGBZ80InstrumentFunctionsPass(PR);
  initializeGBZ80RelaxMemPass(PR);
}

const GBZ80Subtarget *GBZ80TargetMachine::getSubtargetImpl() const {
  return &SubTarget;
}

const GBZ80Subtarget *GBZ80TargetMachine::getSubtargetImpl(const Function &) const {
  return &SubTarget;
}

//===----------------------------------------------------------------------===//
// Pass Pipeline Configuration
//===----------------------------------------------------------------------===//

bool GBZ80PassConfig::addInstSelector() {
  // Install an instruction selector.
  addPass(createGBZ80ISelDag(getGBZ80TargetMachine(), getOptLevel()));
  // Create the frame analyzer pass used by the PEI pass.
 // addPass(createGBZ80FrameAnalyzerPass());

  return false;
}

void GBZ80PassConfig::addPreRegAlloc() {
  // Create the dynalloc SP save/restore pass to handle variable sized allocas.
  //addPass(createGBZ80DynAllocaSRPass());
}

void GBZ80PassConfig::addPreSched2() {
  //addPass(createGBZ80RelaxMemPass());
  //addPass(createGBZ80ExpandPseudoPass());
}

void GBZ80PassConfig::addPreEmitPass() {
  // Must run branch selection immediately preceding the asm printer.
  addPass(&BranchRelaxationPassID);
}

} // end of namespace llvm
