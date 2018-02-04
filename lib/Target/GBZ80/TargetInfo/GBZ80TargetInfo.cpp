//===-- GBZ80TargetInfo.cpp - GBZ80 Target Implementation ---------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/IR/Module.h"
#include "llvm/Support/TargetRegistry.h"
namespace llvm {
Target &getTheGBZ80Target() {
  static Target TheGBZ80Target;
  return TheGBZ80Target;
}
}

extern "C" void LLVMInitializeGBZ80TargetInfo() {
  llvm::RegisterTarget<llvm::Triple::gbz80> X(llvm::getTheGBZ80Target(), "GBZ80",
                                            "Game Boy Z80 (LR35902)");
}

