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

  MachineInstr *expandSimple16(MachineInstr &, unsigned LoOpc, unsigned HiOpc);
  MachineInstr *expandPseudo(MachineInstr &);
  bool expandPseudos();
  bool expandBranch16();
  bool optimizeCP();

};

char GBZ80PostISel::ID = 0;

MachineInstr *GBZ80PostISel::expandSimple16(MachineInstr &MI, unsigned LoOpc,
                                            unsigned HiOpc) {
  /*
  Emit from dst = OP src, src2/imm
    loA = EXTRACT_SUBREG src, sub_lo
    if src2
     loB = EXTRACT_SUBREG src2, sub_lo
     loR = LoOpc loA, loB
    else
     loR = LoOpc loA, LO(imm)
    end
    hiA = EXTRACT_SUBREG src, sub_hi
    if src2
     hiB = EXTRACT_SUBREG src2, sub_hi
     hiR = HiOpc hiA, hiB
    else
     hiR = HiOpc hiA, HI(imm)
    end
    idef = IMPLICIT_DEF
    iA = INSERT_SUBREG idef, loR, sub_lo
    iR = INSERT_SUBREG iA, hiR, sub_hi
  */

  DebugLoc dl = MI.getDebugLoc();
  unsigned PairDst = MI.getOperand(0).getReg();
  unsigned PairSrc = MI.getOperand(1).getReg();
  bool RegOp = MI.getOperand(2).isReg();
  unsigned PairSrc2 = RegOp ? MI.getOperand(2).getReg() : 0;
  int8_t ImmLo = !RegOp ? (int8_t)MI.getOperand(2).getImm() : 0;
  int8_t ImmHi = !RegOp ? (int8_t)(MI.getOperand(2).getImm() >> 8) : 0;

  unsigned loAGPR = MRI->createVirtualRegister(&GB::GPR8RegClass);
  BuildMI(*MI.getParent(), MI, dl, TII->get(GB::COPY), loAGPR)
    .addReg(PairSrc, 0, GB::sub_lo);

  unsigned hiAGPR = MRI->createVirtualRegister(&GB::GPR8RegClass);
  BuildMI(*MI.getParent(), MI, dl, TII->get(GB::COPY), hiAGPR)
    .addReg(PairSrc, 0, GB::sub_hi);

  unsigned loB;
  unsigned hiB;
  if (RegOp) {
    loB = MRI->createVirtualRegister(&GB::GPR8RegClass);
    hiB = MRI->createVirtualRegister(&GB::GPR8RegClass);
    BuildMI(*MI.getParent(), MI, dl, TII->get(GB::COPY), loB)
      .addReg(PairSrc2, 0, GB::sub_lo);
    BuildMI(*MI.getParent(), MI, dl, TII->get(GB::COPY), hiB)
      .addReg(PairSrc2, 0, GB::sub_hi);
  }

  unsigned loA = MRI->createVirtualRegister(&GB::ARegRegClass);
  BuildMI(*MI.getParent(), MI, dl, TII->get(GB::COPY), loA)
    .addReg(loAGPR);

  unsigned loR = MRI->createVirtualRegister(&GB::ARegRegClass);
  if (RegOp) {
    BuildMI(*MI.getParent(), MI, dl, TII->get(LoOpc), loR)
      .addReg(loA)
      .addReg(loB);
  } else {
    BuildMI(*MI.getParent(), MI, dl, TII->get(LoOpc), loR)
      .addReg(loA)
      .addImm(ImmLo);
  }

  unsigned loGPR = MRI->createVirtualRegister(&GB::GPR8RegClass);
  BuildMI(*MI.getParent(), MI, dl, TII->get(GB::COPY), loGPR)
    .addReg(loR);

  unsigned Imp1 = MRI->createVirtualRegister(&GB::PairsRegClass);
  BuildMI(*MI.getParent(), MI, dl, TII->get(GB::IMPLICIT_DEF), Imp1);

  unsigned Imp2 = MRI->createVirtualRegister(&GB::PairsRegClass);
  BuildMI(*MI.getParent(), MI, dl, TII->get(GB::INSERT_SUBREG), Imp2)
    .addReg(Imp1)
    .addReg(loGPR)
    .addImm(GB::sub_lo);

  unsigned hiA = MRI->createVirtualRegister(&GB::ARegRegClass);
  BuildMI(*MI.getParent(), MI, dl, TII->get(GB::COPY), hiA)
    .addReg(hiAGPR);

  unsigned hiR = MRI->createVirtualRegister(&GB::ARegRegClass);
  if (RegOp) {
    BuildMI(*MI.getParent(), MI, dl, TII->get(HiOpc), hiR)
      .addReg(hiA)
      .addReg(hiB);
  } else {
    BuildMI(*MI.getParent(), MI, dl, TII->get(HiOpc), hiR)
      .addReg(hiA)
      .addImm(ImmHi);
  }

  unsigned hiGPR = MRI->createVirtualRegister(&GB::GPR8RegClass);
  BuildMI(*MI.getParent(), MI, dl, TII->get(GB::COPY), hiGPR)
    .addReg(hiR);

  unsigned Imp3 = MRI->createVirtualRegister(&GB::PairsRegClass);
  BuildMI(*MI.getParent(), MI, dl, TII->get(GB::INSERT_SUBREG), Imp3)
    .addReg(Imp2)
    .addReg(hiGPR)
    .addImm(GB::sub_hi);

  MachineInstr *I = BuildMI(*MI.getParent(), MI, dl, TII->get(GB::COPY),
                            PairDst)
    .addReg(Imp3);

  return I;
}

MachineInstr *GBZ80PostISel::expandPseudo(MachineInstr &MI) {
  switch (MI.getOpcode()) {
  default: break;
  case GB::ADD16r:
    return expandSimple16(MI, GB::ADD_r, GB::ADC_r);
  case GB::ADD16i:
    return expandSimple16(MI, GB::ADD_n, GB::ADC_n);
  case GB::ADC16r:
    return expandSimple16(MI, GB::ADC_r, GB::ADC_r);
  case GB::ADC16i:
    return expandSimple16(MI, GB::ADC_n, GB::ADC_n);
  case GB::SUB16r:
    return expandSimple16(MI, GB::SUB_r, GB::SBC_r);
  case GB::SUB16i:
    return expandSimple16(MI, GB::SUB_n, GB::SBC_n);
  case GB::SBC16r:
    return expandSimple16(MI, GB::SBC_r, GB::SBC_r);
  case GB::SBC16i:
    return expandSimple16(MI, GB::SBC_n, GB::SBC_n);

  case GB::AND16r:
    return expandSimple16(MI, GB::AND_r, GB::AND_r);
  case GB::AND16i:
    return expandSimple16(MI, GB::AND_n, GB::AND_n);

  case GB::OR16r:
    return expandSimple16(MI, GB::OR_r, GB::OR_r);
  case GB::OR16i:
    return expandSimple16(MI, GB::OR_n, GB::OR_n);

  case GB::XOR16r:
    return expandSimple16(MI, GB::XOR_r, GB::XOR_r);
  case GB::XOR16i:
    return expandSimple16(MI, GB::XOR_n, GB::XOR_n);

  }
  return nullptr;
}

bool GBZ80PostISel::expandPseudos() {
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

  Modified |= expandPseudos();

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
