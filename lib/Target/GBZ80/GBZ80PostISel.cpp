//===--------- GBZ80PostISel.cpp - Post ISel pass -------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains a pass that runs after instruction selection.
//
//===----------------------------------------------------------------------===//

#include "GBZ80.h"
#include "GBZ80InstrInfo.h"
#include "GBZ80TargetMachine.h"
#include "MCTargetDesc/GBZ80MCTargetDesc.h"

#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/Target/TargetRegisterInfo.h"

using namespace llvm;

#define PASS_NAME "GBZ80 post isel"
#define DEBUG_TYPE "gbz80-post-isel"

namespace {

class GBZ80PostISel : public MachineFunctionPass {
public:
  static char ID;

  GBZ80PostISel() : MachineFunctionPass(ID) {
    initializeGBZ80PostISelPass(*PassRegistry::getPassRegistry());
  }

  bool runOnMachineFunction(MachineFunction &MF) override;

  StringRef getPassName() const override { return PASS_NAME; }

private:
  const GBZ80RegisterInfo *TRI;
  const TargetInstrInfo *TII;
  MachineRegisterInfo *MRI;

  MachineFunction *MF;

  bool expandBranch16();
  bool optimizeCP();

};

char GBZ80PostISel::ID = 0;


bool GBZ80PostISel::optimizeCP() {
  bool Modified = false;


  return Modified;
}

bool GBZ80PostISel::expandBranch16() {
  bool Modified = false;

  // Expand BR16


  return Modified;
}

bool GBZ80PostISel::runOnMachineFunction(MachineFunction &MF) {
  bool Modified = false;

  this->MF = &MF;
  const GBZ80Subtarget &STI = MF.getSubtarget<GBZ80Subtarget>();
  TRI = STI.getRegisterInfo();
  TII = STI.getInstrInfo();
  MRI = &MF.getRegInfo();

  // Expand branch and select pseudos:
  //  * BR16
  //  * Select16_8
  //  * Select16_16
  Modified |= expandBranch16();

  // Optimize compares by swapping operands.
  Modified |= optimizeCP();

  return Modified;
}

} // end of anonymous namespace

INITIALIZE_PASS(GBZ80PostISel, DEBUG_TYPE,
                PASS_NAME, false, false)
namespace llvm {

FunctionPass *createGBZ80PostISelPass() { return new GBZ80PostISel(); }

} // end of namespace llvm
