//===--------- GBZ80PreRA.cpp - Pre RA pass -------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains a pass that runs before register allocation.
//
//===----------------------------------------------------------------------===//

#include "GBZ80.h"
#include "GBZ80InstrInfo.h"
#include "GBZ80TargetMachine.h"
#include "MCTargetDesc/GBZ80MCTargetDesc.h"

#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/Target/TargetRegisterInfo.h"

using namespace llvm;

#define PASS_NAME "GBZ80 pre RA"
#define DEBUG_TYPE "gbz80-pre-ra"

namespace {

class GBZ80PreRA : public MachineFunctionPass {
public:
  static char ID;

  GBZ80PreRA() : MachineFunctionPass(ID) {
    initializeGBZ80PreRAPass(*PassRegistry::getPassRegistry());
  }

  bool runOnMachineFunction(MachineFunction &MF) override;

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesCFG();
    MachineFunctionPass::getAnalysisUsage(AU);
    AU.addRequired<MachineDominatorTree>();
  }

  StringRef getPassName() const override { return PASS_NAME; }

private:
  typedef MachineBasicBlock Block;
  typedef Block::iterator BlockIt;

  const GBZ80RegisterInfo *TRI;
  const TargetInstrInfo *TII;
  MachineRegisterInfo *MRI;
  MachineFunction *MF;
  MachineDominatorTree *DT;

  bool combinePostIncMemAccs();

};

char GBZ80PreRA::ID = 0;


// TODO: this would be better if we could trace identical vregs through copy.
/// Returns true if MI dominates all the uses of Reg.
static bool dominatesAllUsesOf(MachineInstr &MI, unsigned Reg,
                               MachineDominatorTree *DT,
                               MachineRegisterInfo *MRI) {
  for (auto &UI : MRI->use_instructions(Reg)) {
    // Skip phis that dominate us. This could be dangerous...
    if (UI.isPHI() && DT->dominates(&UI, &MI))
      continue;
    if (!DT->dominates(&MI, &UI))
      return false;
  }
  return true;
}

bool GBZ80PreRA::combinePostIncMemAccs() {
  bool Modified = false;

  for (auto MBBI = MF->begin(); MBBI != MF->end(); ++MBBI) {
    for (auto MII = MBBI->begin(); MII != MBBI->end(); ++MII) {
      MachineInstr &MemMI = *MII;
      if (MemMI.getOpcode() != GB::LD8 &&
          MemMI.getOpcode() != GB::ST8)
        continue;
      unsigned PtrReg = MemMI.getOperand(1).getReg();
      MachineInstr *IncMI = nullptr;
      for (auto UI = MRI->use_instr_begin(PtrReg); UI != MRI->use_instr_end();
        ++UI) {
        // Ignore the load/store use.
        if (&*UI == &MemMI)
          continue;
        // If there's a use that isn't an inc-instr, reset and break.
        if (UI->getOpcode() != GB::INC_ss &&
            UI->getOpcode() != GB::DEC_ss) {
          IncMI = nullptr;
          break;
        }
        // If we already found one, we can't have another. Reset and break.
        if (IncMI) {
          IncMI = nullptr;
          break;
        }
        // If we don't have an inc-instr and this is one, set it.
        IncMI = &*UI;
      }
      if (!IncMI)
        continue;
      unsigned IncDef = IncMI->getOperand(0).getReg();
      // If MemMI dominates all uses of the inc-def register, it's safe to
      // combine.
      if (!dominatesAllUsesOf(MemMI, IncDef, DT, MRI))
        continue;

      // We can combine. Replace:
      //  %dst = LD8 %ptr
      // with
      //  %dst, %post = LD8_INC %ptr
      // and replace all IncDef with %post.
      unsigned opc;
      MachineInstr *NewMI;
      if (MemMI.getOpcode() == GB::LD8) {
        opc = IncMI->getOpcode() == GB::INC_ss ? GB::LD8_INC : GB::LD8_DEC;
        // The load def cannot be a subreg due to AReg.
        NewMI =
          BuildMI(*MBBI, MemMI, DebugLoc(), TII->get(opc),
            MemMI.getOperand(0).getReg())
          .addReg(IncDef, RegState::Define)
          .addReg(PtrReg);
      } else {
        opc = IncMI->getOpcode() == GB::INC_ss ? GB::ST8_INC : GB::ST8_DEC;
        NewMI =
          BuildMI(*MBBI, MemMI, DebugLoc(), TII->get(opc),
            IncDef)
          .addReg(MemMI.getOperand(0).getReg())
          .addReg(PtrReg);
      }
      MemMI.eraseFromParent();
      IncMI->eraseFromBundle();
      MII = MBBI->begin();
      Modified = true;
    }
  }

  return Modified;
}

bool GBZ80PreRA::runOnMachineFunction(MachineFunction &MF) {
  bool Modified = false;

  this->MF = &MF;
  const GBZ80Subtarget &STI = MF.getSubtarget<GBZ80Subtarget>();
  TRI = STI.getRegisterInfo();
  TII = STI.getInstrInfo();
  MRI = &MF.getRegInfo();
  DT = &getAnalysis<MachineDominatorTree>();

  // Combine LD8/ST8 with INC/DEC to form LD/ST_INC/DEC.
  Modified |= combinePostIncMemAccs();

  return Modified;
}

} // end of anonymous namespace

INITIALIZE_PASS_BEGIN(GBZ80PreRA, DEBUG_TYPE,
                      PASS_NAME, false, false)
INITIALIZE_PASS_DEPENDENCY(MachineDominatorTree)
INITIALIZE_PASS_END(GBZ80PreRA, DEBUG_TYPE,
                    PASS_NAME, false, false)
namespace llvm {

FunctionPass *createGBZ80PreRAPass() { return new GBZ80PreRA(); }

} // end of namespace llvm
