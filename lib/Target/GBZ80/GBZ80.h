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
FunctionPass *createGBZ80PostISelPass();
FunctionPass *createGBZ80PreRAPass();
FunctionPass *createGBZ80PostRAPass();
FunctionPass *createGBZ80PostPEIPass();
FunctionPass *createGBZ80PreEmitPass();

void initializeGBZ80PostISelPass(PassRegistry&);
void initializeGBZ80PreRAPass(PassRegistry&);
void initializeGBZ80PostRAPass(PassRegistry&);
void initializeGBZ80PostPEIPass(PassRegistry&);
void initializeGBZ80PreEmitPass(PassRegistry&);

/// Contains the GBZ80 backend.
namespace GB {

} // end of namespace GB

} // end namespace llvm

#endif // LLVM_GBZ80_H
