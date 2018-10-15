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
#include "llvm/CodeGen/TargetRegisterInfo.h"

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
  MachineInstr *expand8BitGlobalLDST(MachineInstr &);
  MachineInstr *expand8BitArith(MachineInstr &, unsigned NewOpc);
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
  unsigned PtrReg, bool PostPtrDead, bool &IsPostOpc) {
  IsPostOpc = false;
  if (isStore) {
    if (PtrReg == GB::RHL && !PostPtrDead && (isPostInc || isPostDec)) {
      IsPostOpc = true;
      return isPostInc ? GB::LD_HLI_A : GB::LD_HLD_A;
    }
    return GB::LD_dd_r;
  } else {
    if (PtrReg == GB::RHL && !PostPtrDead && (isPostInc || isPostDec)) {
      IsPostOpc = true;
      return isPostInc ? GB::LD_A_HLI : GB::LD_A_HLD;
    }
    return GB::LD_r_dd;
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
    getOpcodeforLDST(isStore, isPostInc, isPostDec, InPtrReg, PostPtrDead, isPostOpc);

  // Assemble the instruction. Postloads have the operands swapped.
  // No need to emit postupdate if it's dead.
  // Be sure to respect restrictions.
  //  if ptr == HL && !post then any op value reg is fine.
  //  Otherwise, the op value reg must be A.
  // FIXME: Do we need to update some kind of liveinterval if we do this?
  unsigned OpValueReg = (InPtrReg == GB::RHL && !isPostOpc) ? ValueReg : GB::RA;

  if (isStore && OpValueReg != ValueReg)
    // Copy to A before storing.
    // FIXME: Flags? Liveranges?
    BuildMI(*MI.getParent(), MI, DebugLoc(), TII->get(GB::COPY), OpValueReg)
      .addReg(ValueReg);

  auto Builder = BuildMI(*MI.getParent(), MI, DebugLoc(), TII->get(opcode));
  if (isPostOpc && !isStore)
    // For postop loads, add both the value and postptr defs.
    Builder.addReg(OpValueReg, RegState::Define)
           .addReg(PostPtrReg, RegState::Define | getDeadRegState(PostPtrDead));
  else {
    // For stores and normal loads, if it's postop, add the def, otherwise
    // just add the value operand (either def or use).
    if (isPostOpc)
      Builder.addReg(PostPtrReg, RegState::Define | getDeadRegState(PostPtrDead));
    Builder.addReg(OpValueReg, getDefRegState(!isStore));
  }
  // Don't add the kill flag if we're going to postupdate manually.
  Builder.addReg(InPtrReg, getKillRegState(InPtrKill &&
                                           !(isPost && !isPostOpc &&
                                             !PostPtrDead)));
  Builder.cloneMemRefs(MI);
  New = Builder;

  if (!isStore && OpValueReg != ValueReg) {
    // Copy from A after loading.
    // FIXME: does it matter if we do this before or after the postupdate below?
    BuildMI(*MI.getParent(), MI, DebugLoc(), TII->get(GB::COPY), ValueReg)
      .addReg(OpValueReg, getKillRegState(true));
  }

  if (isPost && !isPostOpc && !PostPtrDead) {
    // If this is a postop but we didn't get a post opcode, we need to emit the
    // postupdate separately, unless the postupdate def is dead.
    opcode = isPostInc ? GB::INC_ss : GB::DEC_ss;
    Builder =
      BuildMI(*MI.getParent(), MI, DebugLoc(), TII->get(opcode))
      .addReg(PostPtrReg, RegState::Define)
      .addReg(InPtrReg, getKillRegState(InPtrKill));
    New = Builder;
  }
  return New;
}

MachineInstr *GBZ80PostRA::expand8BitGlobalLDST(MachineInstr &MI) {
  bool isStore = MI.mayStore();

  unsigned ValueReg = MI.getOperand(0).getReg();
  MachineOperand &Addr = MI.getOperand(1);

  if (isStore)
    // Copy to A before storing.
    // FIXME: Flags? Liveranges?
    BuildMI(*MI.getParent(), MI, DebugLoc(), TII->get(GB::COPY), GB::RA)
      .addReg(ValueReg);

  unsigned opcode = isStore ? GB::LD_nn_A : GB::LD_A_nn;
  auto Builder =
    BuildMI(*MI.getParent(), MI, DebugLoc(), TII->get(opcode))
      .addReg(GB::RA, getDefRegState(!isStore));
  if (Addr.isGlobal())
    Builder.addGlobalAddress(Addr.getGlobal(), Addr.getOffset());
  else if (Addr.isImm())
    Builder.addImm(Addr.getImm());
  Builder.cloneMemRefs(MI);

  MachineInstr *New = Builder;
  if (!isStore)
    // Copy from A after loading.
    New = BuildMI(*MI.getParent(), MI, DebugLoc(), TII->get(GB::COPY),
                  ValueReg)
            .addReg(GB::RA, getKillRegState(true));
  return New;
}

MachineInstr *GBZ80PostRA::expand8BitArith(MachineInstr &MI,
                                           unsigned NewOpc) {
  unsigned DstReg = MI.getOperand(0).getReg();
  bool DstIsDead = MI.getOperand(0).isDead();
  unsigned RHSIsReg = MI.getOperand(2).isReg();
  unsigned RHSReg = RHSIsReg ? MI.getOperand(2).getReg() : 0;
  bool RHSIsKill = RHSIsReg ? MI.getOperand(2).isKill() : false;
  int8_t RHSImm = !RHSIsReg ? MI.getOperand(2).getImm() : 0;
  assert(RHSReg != GB::RA);
  // Copy the LHS to A. It won't be A at this point.
  // XXX: Isn't this guaranteed to be kill because of the tie?
  BuildMI(*MI.getParent(), MI, DebugLoc(), TII->get(GB::COPY), GB::RA)
    .addReg(DstReg);

  // If the def is dead in the original instr, that means it's dead on
  // this one. The RA use is kill.
  auto &B = BuildMI(*MI.getParent(), MI, DebugLoc(), TII->get(NewOpc))
    .addReg(GB::RA, RegState::Define | getDeadRegState(DstIsDead))
    .addReg(GB::RA, RegState::Kill);
  if (RHSIsReg)
    B.addReg(RHSReg, getKillRegState(RHSIsKill));
  else
    B.addImm(RHSImm);

  MachineInstr *New = B;
  // If the dst isn't dead, copy it back to the real dst. The RA
  // use is kill.
  if (!DstIsDead)
    New = BuildMI(*MI.getParent(), MI, DebugLoc(), TII->get(GB::COPY))
      .addReg(DstReg, RegState::Define)
      .addReg(GB::RA, RegState::Kill);

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

  case GB::LD8_nn:
  case GB::ST8_nn:
    return expand8BitGlobalLDST(MI);

  case GB::ADD8r:
    return expand8BitArith(MI, GB::ADD_r);
  case GB::ADD8i:
    return expand8BitArith(MI, GB::ADD_n);

  case GB::ADC8r:
    return expand8BitArith(MI, GB::ADC_r);
  case GB::ADC8i:
    return expand8BitArith(MI, GB::ADC_n);

  case GB::SUB8r:
    return expand8BitArith(MI, GB::SUB_r);
  case GB::SUB8i:
    return expand8BitArith(MI, GB::SUB_n);

  case GB::SBC8r:
    return expand8BitArith(MI, GB::SBC_r);
  case GB::SBC8i:
    return expand8BitArith(MI, GB::SBC_n);

  case GB::AND8r:
    return expand8BitArith(MI, GB::AND_r);
  case GB::AND8i:
    return expand8BitArith(MI, GB::AND_n);

  case GB::OR8r:
    return expand8BitArith(MI, GB::OR_r);
  case GB::OR8i:
    return expand8BitArith(MI, GB::OR_n);

  case GB::XOR8r:
    return expand8BitArith(MI, GB::XOR_r);
  case GB::XOR8i:
    return expand8BitArith(MI, GB::XOR_n);

  case GB::CP8r:
  case GB::CP8i: {
    // Handle CP separately as it's a special case (no def)
    unsigned NewOpc = MI.getOpcode() == GB::CP8r ? GB::CP_r : GB::CP_n;
    unsigned LHSReg = MI.getOperand(0).getReg();
    unsigned RHSIsReg = MI.getOperand(1).isReg();
    unsigned RHSReg = RHSIsReg ? MI.getOperand(1).getReg() : 0;
    bool RHSIsKill = RHSIsReg ? MI.getOperand(1).isKill() : false;
    int8_t RHSImm = !RHSIsReg ? MI.getOperand(1).getImm() : 0;

    // Copy the LHS to A. It won't be A at this point.
    BuildMI(*MI.getParent(), MI, DebugLoc(), TII->get(GB::COPY), GB::RA)
      .addReg(LHSReg);

    auto &B = BuildMI(*MI.getParent(), MI, DebugLoc(), TII->get(NewOpc))
      .addReg(GB::RA, RegState::Kill);
    if (RHSIsReg)
      B.addReg(RHSReg, getKillRegState(RHSIsKill));
    else
      B.addImm(RHSImm);

    return B;
  }

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
        Modified = true;
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
