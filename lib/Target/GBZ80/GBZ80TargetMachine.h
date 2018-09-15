//===-- GBZ80TargetMachine.h - Define TargetMachine for GBZ80 -------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares the GBZ80 specific subclass of TargetMachine.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_GBZ80_TARGET_MACHINE_H
#define LLVM_GBZ80_TARGET_MACHINE_H

#include "llvm/IR/DataLayout.h"
#include "llvm/Target/TargetMachine.h"

#include "GBZ80FrameLowering.h"
#include "GBZ80ISelLowering.h"
#include "GBZ80InstrInfo.h"
#include "GBZ80SelectionDAGInfo.h"
#include "GBZ80Subtarget.h"

namespace llvm {

/// A generic GBZ80 implementation.
class GBZ80TargetMachine : public LLVMTargetMachine {
public:
  GBZ80TargetMachine(const Target &T, const Triple &TT, StringRef CPU,
                   StringRef FS, const TargetOptions &Options, Optional<Reloc::Model> RM,
                   Optional<CodeModel::Model> CM, CodeGenOpt::Level OL, bool JIT);

  const GBZ80Subtarget *getSubtargetImpl() const;
  const GBZ80Subtarget *getSubtargetImpl(const Function &) const override;

  TargetLoweringObjectFile *getObjFileLowering() const override {
    return this->TLOF.get();
  }

  TargetPassConfig *createPassConfig(PassManagerBase &PM) override;

  bool isMachineVerifierClean() const override {
    return false;
  }

private:
  std::unique_ptr<TargetLoweringObjectFile> TLOF;
  GBZ80Subtarget SubTarget;
};

} // end namespace llvm

#endif // LLVM_GBZ80_TARGET_MACHINE_H
