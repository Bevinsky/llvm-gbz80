//===--------- GBZ80PostRA.cpp - Post RA pass -------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains a pass that runs after register allocation.
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

#define PASS_NAME "GBZ80 post RA"
#define DEBUG_TYPE "gbz80-post-ra"

namespace {

class GBZ80PostRA : public MachineFunctionPass {
public:
  static char ID;

  GBZ80PostRA() : MachineFunctionPass(ID) {
    initializeGBZ80PostRAPass(*PassRegistry::getPassRegistry());
  }

  bool runOnMachineFunction(MachineFunction &MF) override;

  StringRef getPassName() const override { return PASS_NAME; }

private:
  typedef MachineBasicBlock Block;
  typedef Block::iterator BlockIt;

  const GBZ80RegisterInfo *TRI;
  const TargetInstrInfo *TII;
  MachineRegisterInfo *MRI;
  MachineFunction *MF;

  bool expandPostRAPseudos();
  MachineInstr *expandPseudo(MachineInstr &);
  MachineInstr *expand8BitLDST(MachineInstr &);
};

char GBZ80PostRA::ID = 0;


// Stores:
// LD_HL_r
// LD_HLI_A
// LD_HLD_A
// LD_dd_A
//
// Loads:
// LD_r_HL
// LD_A_HLI
// LD_A_HLD
// LD_A_dd
static unsigned getOpcodeforLDST(bool isStore, bool isPostInc, bool isPostDec,
  unsigned PtrReg, bool &IsPostOpc) {
  IsPostOpc = false;
  if (isStore) {
    if (PtrReg != GB::rHL)
      return GB::LD_dd_A;
    else if (isPostInc) {
      IsPostOpc = true;
      return GB::LD_HLI_A;
    } else if (isPostDec) {
      IsPostOpc = true;
      return GB::LD_HLD_A;
    }
    return GB::LD_HL_r;
  } else {
    if (PtrReg != GB::rHL)
      return GB::LD_A_dd;
    else if (isPostInc) {
      IsPostOpc = true;
      return GB::LD_A_HLI;
    } else if (isPostDec) {
      IsPostOpc = true;
      return GB::LD_A_HLD;
    }
    return GB::LD_r_HL;
  }
}

MachineInstr *GBZ80PostRA::expand8BitLDST(MachineInstr &MI) {
  bool isStore = MI.mayStore();
  bool isPostInc =
    MI.getOpcode() == GB::LD8_INC || MI.getOpcode() == GB::ST8_INC;
  bool isPostDec =
    MI.getOpcode() == GB::LD8_DEC || MI.getOpcode() == GB::ST8_DEC;
  bool isPost = isPostInc || isPostDec;

  // Input ptr is always 1; 2 for postops.
  unsigned InPtrReg = MI.getOperand(1 + isPost).getReg();
  bool InPtrKill = MI.getOperand(1 + isPost).isKill();
  // Value is in 0, 1 for poststore.
  unsigned ValueReg = MI.getOperand(0 + (isPost && isStore)).getReg();
  // Post ptr is in 0 for store, 1 for load.
  unsigned PostPtrReg = 0;
  bool PostPtrDead = false;
  if (isPost) {
    PostPtrReg = MI.getOperand(0 + (isPost && !isStore)).getReg();
    PostPtrDead = MI.getOperand(0 + (isPost && !isStore)).isDead();
  }

  MachineInstr *New = nullptr;
  bool isPostOpc;
  unsigned opcode =
    getOpcodeforLDST(isStore, isPostInc, isPostDec, InPtrReg, isPostOpc);
  // Assemble the instruction. Postloads have the operands swapped.
  // No need to emit postupdate if it's dead.
  auto Builder = BuildMI(*MI.getParent(), MI, DebugLoc(), TII->get(opcode));
  if (isPostOpc && !PostPtrDead && !isStore)
    Builder.addReg(PostPtrReg, RegState::Define)
           .addReg(ValueReg, RegState::Define);
  else {
    if (isPostOpc && !PostPtrDead)
      Builder.addReg(PostPtrReg, RegState::Define);
    Builder.addReg(ValueReg, getDefRegState(isStore));
  }
  // Don't add the kill flag if we're going to postupdate manually.
  Builder.addReg(InPtrReg, getKillRegState(InPtrKill &&
                                           !(isPost && !isPostOpc &&
                                             !PostPtrDead)));
  for (auto &MMO : MI.memoperands())
    Builder.addMemOperand(MMO);
  New = Builder;
  if (isPost && !isPostOpc && !PostPtrDead) {
    // If this is a postop but we didn't get a post opcode, we need to emit the
    // postupdate separately, unless the postupdate def is dead.
    opcode = isPostInc ? GB::INC_ss : GB::DEC_ss;
    Builder =
      BuildMI(*MI.getParent(), MI, DebugLoc(), TII->get(opcode))
      .addReg(PostPtrReg, RegState::Define)
      .addReg(InPtrReg);
    New = Builder;
  }
  return New;
}

MachineInstr *GBZ80PostRA::expandPseudo(MachineInstr &MI) {
  MachineInstr *LastNew = nullptr;
  switch (MI.getOpcode()) {
  default: break;
  case GB::LD8: 
  case GB::ST8:
  case GB::LD8_INC:
  case GB::ST8_INC:
  case GB::LD8_DEC:
  case GB::ST8_DEC:
    return expand8BitLDST(MI);
  }

  return LastNew;
}

bool GBZ80PostRA::expandPostRAPseudos() {
  bool Modified = false;

  for (auto &MBB : *MF) {
    for (auto MII = MBB.begin(); MII != MBB.end(); ) {
      MachineInstr *New = expandPseudo(*MII);
      if (New) {
        MII->eraseFromParent();
        MII = New->getIterator();
      }
      ++MII;
    }
  }

  return Modified;
}

bool GBZ80PostRA::runOnMachineFunction(MachineFunction &MF) {
  bool Modified = false;

  this->MF = &MF;
  const GBZ80Subtarget &STI = MF.getSubtarget<GBZ80Subtarget>();
  TRI = STI.getRegisterInfo();
  TII = STI.getInstrInfo();
  MRI = &MF.getRegInfo();

  Modified |= expandPostRAPseudos();

  return Modified;
}

} // end of anonymous namespace

INITIALIZE_PASS(GBZ80PostRA, DEBUG_TYPE,
                PASS_NAME, false, false)
namespace llvm {

FunctionPass *createGBZ80PostRAPass() { return new GBZ80PostRA(); }

} // end of namespace llvm
