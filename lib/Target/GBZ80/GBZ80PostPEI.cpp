//===--------- GBZ80PostPEI.cpp - Post PEI pass ---------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains a pass that runs after prolog/epilog insertion.
//
//===----------------------------------------------------------------------===//

#include "GBZ80.h"
#include "GBZ80InstrInfo.h"
#include "GBZ80TargetMachine.h"
#include "MCTargetDesc/GBZ80MCTargetDesc.h"

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/LiveIntervals.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"

#include <deque>

using namespace llvm;

#define PASS_NAME "GBZ80 post PEI"
#define DEBUG_TYPE "gbz80-post-pei"

namespace {

typedef std::pair<VNInfo *, VNInfo *> VNPair;

struct FrameAccessInfo {
  MachineInstr *MI;
  unsigned StackAdjustment;
  bool MustSaveHL;
  FrameAccessInfo *ChainedAccess;
  FrameAccessInfo *PreviousAccess;
};

class GBZ80PostPEI : public MachineFunctionPass {
public:
  static char ID;

  GBZ80PostPEI() : MachineFunctionPass(ID) {
    initializeGBZ80PreEmitPass(*PassRegistry::getPassRegistry());
  }

  bool runOnMachineFunction(MachineFunction &MF) override;

  StringRef getPassName() const override { return PASS_NAME; }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    MachineFunctionPass::getAnalysisUsage(AU);
    AU.addRequired<LiveIntervals>();
  }

private:
  typedef MachineBasicBlock Block;
  typedef Block::iterator BlockIt;

  MachineFunction *MF;
  const GBZ80RegisterInfo *TRI;
  const TargetInstrInfo *TII;
  MachineRegisterInfo *MRI;
  LiveIntervals *LI;
  RegScavenger *RS;

  SmallVector<FrameAccessInfo, 16> Accesses;
  DenseMap<MachineBasicBlock *, unsigned> OutgoingAdjustments;

  MachineOperand *getIndexOperand(MachineInstr *);
  VNPair getVNsBefore(MachineInstr *, unsigned);
  VNPair getVNsAfter(MachineInstr *, unsigned);
  bool isChainableWithPrevious(MachineInstr *, FrameAccessInfo *);
  int getStackAdjustment(MachineInstr *);
  unsigned getAdjAtBlockEntry(MachineBasicBlock *);

  void CollectFrameAccesses();

  void TransformFrameAccesses();
};

char GBZ80PostPEI::ID = 0;

/// Return the VNInfos before the given instruction for the given register.
VNPair GBZ80PostPEI::getVNsBefore(MachineInstr *MI, unsigned Reg) {
  VNPair P = {nullptr, nullptr};
  bool FirstSet = false;
  SlotIndex Idx = LI->getInstructionIndex(*MI);
  for (MCRegUnitIterator Units(Reg, TRI); Units.isValid(); ++Units) {
    LiveRange &LR = LI->getRegUnit(*Units);
    VNInfo *VN = LR.getVNInfoBefore(Idx);
    if (!FirstSet) {
      P.first = VN;
      FirstSet = true;
    } else
      P.second = VN;
  }
  return P;
}
/// Return the VNInfos after the given instruction for the given register.
VNPair GBZ80PostPEI::getVNsAfter(MachineInstr *MI, unsigned Reg) {
  VNPair P = {nullptr, nullptr};
  bool FirstSet = false;
  SlotIndex Idx = LI->getInstructionIndex(*MI);
  for (MCRegUnitIterator Units(Reg, TRI); Units.isValid(); ++Units) {
    LiveRange &LR = LI->getRegUnit(*Units);
    VNInfo *VN = LR.getVNInfoAt(Idx);
    if (!FirstSet) {
      P.first = VN;
      FirstSet = true;
    } else
      P.second = VN;
  }
  return P;
}

/// Return the operand of the former frame index, if this is a frame access.
MachineOperand *GBZ80PostPEI::getIndexOperand(MachineInstr *MI) {
  switch (MI->getOpcode()) {
  default: break;
  case GB::FRMIDX:
  case GB::LD8_FI:
  case GB::ST8_FI:
    return &MI->getOperand(1);
  }
  return nullptr;
}

/// Return true if the HL save for Cur can be chained from Prev.
bool GBZ80PostPEI::isChainableWithPrevious(MachineInstr *Cur,
                                           FrameAccessInfo *Prev) {
  // TODO
  return false;
}

/// Return the amount by which our local stack will grow by running this
/// instruction.
int GBZ80PostPEI::getStackAdjustment(MachineInstr *MI) {
  switch (MI->getOpcode()) {
  case GB::PUSH:     return 2;
  case GB::POP:      return -2;
  case GB::ADD_SP_e: return -(int8_t)MI->getOperand(0).getImm();
  case GB::INC_ss:
    if (MI->getOperand(0).getReg() == GB::SP)
      return -1;
    break;
  case GB::DEC_ss:
    if (MI->getOperand(0).getReg() == GB::SP)
      return 1;
    break;
  case GB::LD_SP_HL: {
    // Do a really simple analysis to look for an 'ADD HL,SP' and 'LD HL,nn'.
    MachineInstr *Prev = MI->getPrevNode();
    assert(Prev->getOpcode() == GB::ADD_HL_ss &&
           Prev->getOperand(2).getReg() == GB::SP && "Cannot analyze SP adj!");
    Prev = Prev->getPrevNode();
    assert(Prev->getOpcode() == GB::LD_dd_nn && "Cannot analyze SP adj!");
    return -(int16_t)Prev->getOperand(1).getImm();
  }
  default: break;
  }
  return 0;
}

static bool isFrameAccessInstr(MachineInstr *MI) {
  switch (MI->getOpcode()) {
  default: break;
  case GB::FRMIDX:
  case GB::LD8_FI:
  case GB::ST8_FI:
    return true;
  }
  return false;
}

unsigned GBZ80PostPEI::getAdjAtBlockEntry(MachineBasicBlock *BB) {
  // Adjustment on function entry is 0 (after the prologue).
  if (BB == &*BB->getParent()->begin())
    return 0;

  unsigned Adj = ~0U;
  for (const auto Pred : BB->predecessors()) {
    if (OutgoingAdjustments.count(Pred)) {
      Adj = OutgoingAdjustments[Pred];
      break;
    }
  }
  assert(Adj != ~0U && "Visited block but not one of its preds?");
  // Make sure they all match.
  for (const auto Pred : BB->predecessors()) {
    if (OutgoingAdjustments.count(Pred))
      assert(OutgoingAdjustments[Pred] == Adj && "Adj mismatch!");
  }
  return Adj;
}

void GBZ80PostPEI::CollectFrameAccesses() {
  std::deque<MachineBasicBlock *> Worklist;
  DenseSet<MachineBasicBlock *> VisitedBlocks;
  Worklist.push_back(&*MF->begin());

  while (!Worklist.empty()) {
    MachineBasicBlock *BB = Worklist.front();
    Worklist.pop_front();
    unsigned Adj = getAdjAtBlockEntry(BB);
    FrameAccessInfo *Prev = nullptr;

    for (auto MII = BB->begin(), MIE = BB->end(); MII != MIE; ++MII) {
      // Skip prologue/epilogue instructions.
      if (MII->getFlag(MachineInstr::FrameSetup) ||
          MII->getFlag(MachineInstr::FrameDestroy))
        continue;

      int MIAdjust = getStackAdjustment(&*MII);
      if (MIAdjust != 0) {
        assert((int)Adj + MIAdjust >= 0 && "Negative stack adjustment?");
        // Modify the adjustment and continue. No real frame accesses adjust
        // the stack pointer.
        Adj = (int)Adj + MIAdjust;
        continue;
      }

      if (!isFrameAccessInstr(&*MII))
        continue;

      FrameAccessInfo FAI;
      FAI.MI = &*MII;
      FAI.StackAdjustment = Adj;

      // We must save HL if the value number of either H or L are the same
      // before and after the instruction, unless there is nothing live before
      // the instruction at all.
      VNPair Before = getVNsBefore(&*MII, GB::RHL);
      VNPair After = getVNsAfter(&*MII, GB::RHL);
      FAI.MustSaveHL =
        ((Before.first == After.first) ||
         (Before.second == After.second)) &&
        !(Before.first == nullptr && Before.second == nullptr);

      // TODO: Chained HL saves.
      FAI.ChainedAccess = nullptr;
      FAI.PreviousAccess = nullptr;

      Accesses.emplace_back(FAI);
    }

    OutgoingAdjustments[BB] = Adj;
    for (auto Succ : BB->successors()) {
      if (VisitedBlocks.count(Succ)) {
        // Succ has already been visited. Verify that all (other) preds of Succ
        // agree on the adjustment by asking for Succ's adjustment.
        (void)getAdjAtBlockEntry(Succ);
      } else {
        // Otherwise, add it to the worklist.
        Worklist.push_back(Succ);
      }
    }
  }
}

void GBZ80PostPEI::TransformFrameAccesses() {
  for (FrameAccessInfo &FAI : Accesses) {
    // Expanding a frame access consists of parts.
    unsigned Opcode = FAI.MI->getOpcode();
    MachineBasicBlock &BB = *FAI.MI->getParent();
    const DebugLoc &dl = FAI.MI->getDebugLoc();

    //  * Save HL if we are told to and we don't have a prev chain.
    bool SaveHL = FAI.MustSaveHL && !FAI.PreviousAccess;
    if (SaveHL) {
      BuildMI(BB, *FAI.MI, dl, TII->get(GB::PUSH))
        .addReg(GB::RHL);
    }

    // (*) For ST8, copy H/L source to A.
    unsigned ValueReg = FAI.MI->getOperand(0).getReg();
    if (Opcode == GB::ST8_FI && (ValueReg == GB::RH || ValueReg == GB::RL)) {
      BuildMI(BB, *FAI.MI, dl, TII->get(GB::LD_r_r), GB::RA)
        .addReg(ValueReg);
      ValueReg = GB::RA;
    }

    // The offset from the stack pointer is:
    // frame index + offset + stack adjustment (+ 2 if HL was saved)
    int16_t FIdx = FAI.MI->getOperand(1).getImm();
    int16_t FIOffset = FAI.MI->getOperand(2).getImm();
    int16_t Adj = FAI.StackAdjustment;
    int Offset = (int)FIdx + FIOffset + Adj + (SaveHL ? 2 : 0);

    //  * Materialize the frame index:
    if (/*ST->isGB() &&*/ Offset <= 127 && Offset >= -128) {
      //  * Either use the LDHL, SP+e
      BuildMI(BB, *FAI.MI, dl, TII->get(GB::LDHL_SP_e), GB::RHL)
        .addImm(Offset);
    } else {
      //  * Or use a LD HL,nn; ADD HL,SP combo if we have to
      BuildMI(BB, *FAI.MI, dl, TII->get(GB::LD_dd_nn), GB::RHL)
        .addImm((int16_t)Offset);
      BuildMI(BB, *FAI.MI, dl, TII->get(GB::ADD_HL_ss), GB::RHL)
        .addReg(GB::RHL)
        .addReg(GB::SP);
    }

    //  * Perform the operation:
    switch (Opcode) {
    case GB::FRMIDX:
      //   * FRMIDX: Copy to the dst (unless dst is HL)
      if (ValueReg != GB::RHL) {
        unsigned Lo, Hi;
        TRI->splitReg(ValueReg, Lo, Hi);
        BuildMI(BB, *FAI.MI, dl, TII->get(GB::LD_r_r), Lo)
          .addReg(GB::RL);
        BuildMI(BB, *FAI.MI, dl, TII->get(GB::LD_r_r), Hi)
          .addReg(GB::RH);
      }
      break;
    case GB::LD8_FI:
      //   * LD8: Load from HL to the register
      BuildMI(BB, *FAI.MI, dl, TII->get(GB::LD_r_dd), ValueReg)
        .addReg(GB::RHL);
      break;
    case GB::ST8_FI:
      //   * ST8: Store from register (or A if we copied) to HL
      BuildMI(BB, *FAI.MI, dl, TII->get(GB::LD_dd_r))
        .addReg(ValueReg)
        .addReg(GB::RHL);
      break;
    }

    //  * Restore HL (unless we have a chain)
    if (SaveHL && !FAI.ChainedAccess) {
      BuildMI(BB, *FAI.MI, dl, TII->get(GB::POP), GB::RHL);
    }

    FAI.MI->eraseFromParent();
  }
}

bool GBZ80PostPEI::runOnMachineFunction(MachineFunction &MF) {
  bool Modified = false;

  this->MF = &MF;
  const GBZ80Subtarget &STI = MF.getSubtarget<GBZ80Subtarget>();
  TRI = STI.getRegisterInfo();
  TII = STI.getInstrInfo();
  LI = &getAnalysis<LiveIntervals>();
  RS = new RegScavenger();
  Accesses.clear();
  OutgoingAdjustments.clear();

  CollectFrameAccesses();

  for (auto &A : Accesses) {
    A.MI->dump();
    errs() << "Stack adj: " << A.StackAdjustment << "\n";
    errs() << "Must save: " << A.MustSaveHL << "\n\n";
  }

  TransformFrameAccesses();

  delete RS;
  return !Accesses.empty();
}

} // end of anonymous namespace

INITIALIZE_PASS(GBZ80PostPEI, DEBUG_TYPE,
                PASS_NAME, false, false)
namespace llvm {

FunctionPass *createGBZ80PostPEIPass() { return new GBZ80PostPEI(); }

} // end of namespace llvm
