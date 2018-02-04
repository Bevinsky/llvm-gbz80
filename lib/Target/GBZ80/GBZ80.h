//===-- GBZ80.h - Top-level interface for GBZ80 representation ------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the entry points for global functions defined in the LLVM
// GBZ80 back-end.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_GBZ80_H
#define LLVM_GBZ80_H

#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {

class GBZ80TargetMachine;
class FunctionPass;

FunctionPass *createGBZ80ISelDag(GBZ80TargetMachine &TM,
                               CodeGenOpt::Level OptLevel);
FunctionPass *createGBZ80ExpandPseudoPass();
FunctionPass *createGBZ80FrameAnalyzerPass();
FunctionPass *createGBZ80InstrumentFunctionsPass();
FunctionPass *createGBZ80RelaxMemPass();
FunctionPass *createGBZ80DynAllocaSRPass();
FunctionPass *createGBZ80BranchSelectionPass();

void initializeGBZ80ExpandPseudoPass(PassRegistry&);
void initializeGBZ80InstrumentFunctionsPass(PassRegistry&);
void initializeGBZ80RelaxMemPass(PassRegistry&);

/// Contains the GBZ80 backend.
namespace GB {

enum AddressSpace { DataMemory, ProgramMemory };

template <typename T> bool isProgramMemoryAddress(T *V) {
  return cast<PointerType>(V->getType())->getAddressSpace() == ProgramMemory;
}

inline bool isProgramMemoryAccess(MemSDNode const *N) {
  auto V = N->getMemOperand()->getValue();

  return (V != nullptr) ? isProgramMemoryAddress(V) : false;
}

} // end of namespace GB

} // end namespace llvm

#endif // LLVM_GBZ80_H
