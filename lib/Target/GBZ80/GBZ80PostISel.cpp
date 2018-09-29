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
#include "llvm/CodeGen/TargetRegisterInfo.h"

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
  MachineInstr *expandShift16(MachineInstr &, unsigned Opc1, unsigned Subreg1,
                              unsigned Opc2, unsigned Subreg2);
  MachineInstr *expandSelect(MachineInstr &);
  MachineInstr *expandPseudo(MachineInstr &);
  bool expandPseudos();
  bool expandShiftLoop16();
  bool expandBranch16();
  bool optimizeCP();

};

char GBZ80PostISel::ID = 0;

MachineInstr *GBZ80PostISel::expandSimple16(MachineInstr &MI, unsigned LoOpc,
                                            unsigned HiOpc) {
  DebugLoc dl = MI.getDebugLoc();
  unsigned Dst16 = MI.getOperand(0).getReg();
  unsigned LHS16 = MI.getOperand(1).getReg();
  bool RHSIsReg = MI.getOperand(2).isReg();
  unsigned RHS16 = RHSIsReg ? MI.getOperand(2).getReg() : 0;
  int8_t ImmLo = !RHSIsReg ? (int8_t)MI.getOperand(2).getImm() : 0;
  int8_t ImmHi = !RHSIsReg ? (int8_t)(MI.getOperand(2).getImm() >> 8) : 0;

  // Extract LHS:lo and LHS:hi
  unsigned LHSLo = MRI->createVirtualRegister(&GB::R8RegClass);

  BuildMI(*MI.getParent(), MI, dl, TII->get(GB::COPY), LHSLo)
    .addReg(LHS16, 0, GB::sub_lo);

  // Extract RHS:lo and RHS:hi if necessary
  unsigned RHSLo;
  if (RHSIsReg) {
    RHSLo = MRI->createVirtualRegister(&GB::R8RegClass);
    BuildMI(*MI.getParent(), MI, dl, TII->get(GB::COPY), RHSLo)
      .addReg(RHS16, 0, GB::sub_lo);
  }

  // Do the operation. Lo first.
  unsigned ResLo = MRI->createVirtualRegister(&GB::R8RegClass);
  auto &LoOp = BuildMI(*MI.getParent(), MI, dl, TII->get(LoOpc), ResLo)
               .addReg(LHSLo);
  if (RHSIsReg)
    LoOp.addReg(RHSLo);
  else
    LoOp.addImm(ImmLo);

  unsigned LHSHi = MRI->createVirtualRegister(&GB::R8RegClass);
  BuildMI(*MI.getParent(), MI, dl, TII->get(GB::COPY), LHSHi)
    .addReg(LHS16, 0, GB::sub_hi);

  unsigned RHSHi;
  if (RHSIsReg) {
    RHSHi = MRI->createVirtualRegister(&GB::R8RegClass);
    BuildMI(*MI.getParent(), MI, dl, TII->get(GB::COPY), RHSHi)
      .addReg(RHS16, 0, GB::sub_hi);
  }

  // Then Hi.
  unsigned ResHi = MRI->createVirtualRegister(&GB::R8RegClass);
  auto &HiOp = BuildMI(*MI.getParent(), MI, dl, TII->get(HiOpc), ResHi)
      .addReg(LHSHi);
  if (RHSIsReg)
    HiOp.addReg(RHSHi);
  else
    HiOp.addImm(ImmHi);

  // Now combine them.
  unsigned Res16_0 = MRI->createVirtualRegister(&GB::R16RegClass);
  BuildMI(*MI.getParent(), MI, dl, TII->get(GB::IMPLICIT_DEF), Res16_0);

  unsigned Res16_1 = MRI->createVirtualRegister(&GB::R16RegClass);
  BuildMI(*MI.getParent(), MI, dl, TII->get(GB::INSERT_SUBREG), Res16_1)
    .addReg(Res16_0)
    .addReg(ResLo)
    .addImm(GB::sub_lo);

  MachineInstr *I =
    BuildMI(*MI.getParent(), MI, dl, TII->get(GB::INSERT_SUBREG), Dst16)
      .addReg(Res16_1)
      .addReg(ResHi)
      .addImm(GB::sub_hi);

  return I;
}

MachineInstr *GBZ80PostISel::expandShift16(MachineInstr &MI,
    unsigned Opc1, unsigned Subreg1, unsigned Opc2, unsigned Subreg2) {
  DebugLoc dl = MI.getDebugLoc();
  unsigned Dst16 = MI.getOperand(0).getReg();
  unsigned Src16 = MI.getOperand(1).getReg();

  // Extract Src:Subreg1
  unsigned SubSrc1 = MRI->createVirtualRegister(&GB::R8RegClass);
  BuildMI(*MI.getParent(), MI, dl, TII->get(GB::COPY), SubSrc1)
    .addReg(Src16, 0, Subreg1);

  // Extract Src:Subreg2
  unsigned SubSrc2 = MRI->createVirtualRegister(&GB::R8RegClass);
  BuildMI(*MI.getParent(), MI, dl, TII->get(GB::COPY), SubSrc2)
    .addReg(Src16, 0, Subreg2);

  // Do the operation. Opc1 first.
  unsigned Res1 = MRI->createVirtualRegister(&GB::R8RegClass);
  auto &LoOp = BuildMI(*MI.getParent(), MI, dl, TII->get(Opc1), Res1)
               .addReg(SubSrc1);

  // Then Opc2.
  unsigned Res2 = MRI->createVirtualRegister(&GB::R8RegClass);
  auto &HiOp = BuildMI(*MI.getParent(), MI, dl, TII->get(Opc2), Res2)
      .addReg(SubSrc2);

  // Now combine them.
  unsigned Res16_0 = MRI->createVirtualRegister(&GB::R16RegClass);
  BuildMI(*MI.getParent(), MI, dl, TII->get(GB::IMPLICIT_DEF), Res16_0);

  unsigned Res16_1 = MRI->createVirtualRegister(&GB::R16RegClass);
  BuildMI(*MI.getParent(), MI, dl, TII->get(GB::INSERT_SUBREG), Res16_1)
    .addReg(Res16_0)
    .addReg(Res1)
    .addImm(Subreg1);

  MachineInstr *I =
    BuildMI(*MI.getParent(), MI, dl, TII->get(GB::INSERT_SUBREG), Dst16)
      .addReg(Res16_1)
      .addReg(Res2)
      .addImm(Subreg2);

  return I;
}

MachineInstr *GBZ80PostISel::expandSelect(MachineInstr &MI) {
  unsigned Opc = MI.getOpcode();
  DebugLoc dl = MI.getDebugLoc();
  MachineInstr *Prev = MI.getPrevNode();

  // To "insert" a SELECT instruction, we insert the diamond
  // control-flow pattern. The incoming instruction knows the
  // destination vreg to set, the condition code register to branch
  // on, the true/false values to select between, and a branch opcode
  // to use.

  MachineBasicBlock *MBB = MI.getParent();
  const BasicBlock *LLVM_BB = MBB->getBasicBlock();
  MachineBasicBlock *trueMBB = MF->CreateMachineBasicBlock(LLVM_BB);
  MachineBasicBlock *falseMBB = MF->CreateMachineBasicBlock(LLVM_BB);

  if (auto *Fallthrough = MBB->getFallThrough()) {
    // Fix the fallthrough here or the CFG will get messed up after the
    // block insertion.
    BuildMI(MBB, dl, TII->get(GB::JP_nn)).addMBB(Fallthrough);
  }

  MachineFunction::iterator I;
  for (I = MF->begin(); I != MF->end() && &(*I) != MBB; ++I);
  if (I != MF->end()) ++I;
  MF->insert(I, trueMBB);
  MF->insert(I, falseMBB);

  // Transfer remaining instructions and all successors of the current
  // block to the block which will contain the Phi node for the
  // select.
  trueMBB->splice(trueMBB->begin(), MBB,
    std::next(MachineBasicBlock::iterator(MI)), MBB->end());
  trueMBB->transferSuccessorsAndUpdatePHIs(MBB);

  if (Opc == GB::Select8_8 || Opc == GB::Select16_8) {
    // Grab a real GBZ80 CC and make a jump instr.
    GBCC::CondCodes CC = (GBCC::CondCodes)MI.getOperand(3).getImm();
    BuildMI(MBB, dl, TII->get(GB::JP_cc_nn)).addMBB(trueMBB).addImm(CC);
  } else {
    // Take the ISD CC and build a BR16.
    ISD::CondCode CC = (ISD::CondCode)MI.getOperand(3).getImm();
    unsigned LReg = MI.getOperand(4).getReg();
    unsigned LSub = MI.getOperand(4).getSubReg();
    unsigned RReg = MI.getOperand(5).getReg();
    unsigned RSub = MI.getOperand(5).getSubReg();
    BuildMI(MBB, dl, TII->get(GB::BR16))
      .addMBB(trueMBB).addImm(CC).addReg(LReg, 0, LSub).addReg(RReg, 0, RSub);
  }
  BuildMI(MBB, dl, TII->get(GB::JP_nn)).addMBB(falseMBB);
  MBB->addSuccessor(falseMBB);
  MBB->addSuccessor(trueMBB);

  // Unconditionally flow back to the true block
  BuildMI(falseMBB, dl, TII->get(GB::JP_nn)).addMBB(trueMBB);
  falseMBB->addSuccessor(trueMBB);

  // Set up the Phi node to determine where we came from
  BuildMI(*trueMBB, trueMBB->begin(), dl, TII->get(GB::PHI), MI.getOperand(0).getReg())
    .addReg(MI.getOperand(1).getReg())
    .addMBB(MBB)
    .addReg(MI.getOperand(2).getReg())
    .addMBB(falseMBB);

  return Prev;
}

MachineInstr *GBZ80PostISel::expandPseudo(MachineInstr &MI) {
  switch (MI.getOpcode()) {
  default: break;
  case GB::ADD16r:
    return expandSimple16(MI, GB::ADD8r, GB::ADC8r);
  case GB::ADD16i:
    return expandSimple16(MI, GB::ADD8i, GB::ADC8i);
  case GB::ADC16r:
    return expandSimple16(MI, GB::ADC8r, GB::ADC8r);
  case GB::ADC16i:
    return expandSimple16(MI, GB::ADC8i, GB::ADC8i);
  case GB::SUB16r:
    return expandSimple16(MI, GB::SUB8r, GB::SBC8r);
  case GB::SUB16i:
    return expandSimple16(MI, GB::SUB8i, GB::SBC8i);
  case GB::SBC16r:
    return expandSimple16(MI, GB::SBC8r, GB::SBC8r);
  case GB::SBC16i:
    return expandSimple16(MI, GB::SBC8i, GB::SBC8i);

  case GB::AND16r:
    return expandSimple16(MI, GB::AND8r, GB::AND8r);
  case GB::AND16i:
    return expandSimple16(MI, GB::AND8i, GB::AND8i);

  case GB::OR16r:
    return expandSimple16(MI, GB::OR8r, GB::OR8r);
  case GB::OR16i:
    return expandSimple16(MI, GB::OR8i, GB::OR8i);

  case GB::XOR16r:
    return expandSimple16(MI, GB::XOR8r, GB::XOR8r);
  case GB::XOR16i:
    return expandSimple16(MI, GB::XOR8i, GB::XOR8i);

  case GB::SHL16:
    return expandShift16(MI, GB::SLA_r, GB::sub_lo, GB::RL_r, GB::sub_hi);
  case GB::LSR16:
    return expandShift16(MI, GB::SRL_r, GB::sub_hi, GB::RR_r, GB::sub_lo);
  case GB::ASR16:
    return expandShift16(MI, GB::SRA_r, GB::sub_hi, GB::RR_r, GB::sub_lo);

  case GB::Select8_8:
  case GB::Select8_16:
  case GB::Select16_8:
  case GB::Select16_16:
    return expandSelect(MI);
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
        Modified = true;
      }
      ++MII;
    }
  }
  return Modified;
}

bool GBZ80PostISel::expandShiftLoop16() {
  bool Modified = false;

  for (auto MBBI = MF->begin(); MBBI != MF->end(); ++MBBI ) {
    for (auto MII = MBBI->begin(); MII != MBBI->end(); ) {
      MachineBasicBlock *MBB = &*MBBI;
      MachineInstr &MI = *MII;
      DebugLoc dl = MI.getDebugLoc();

      unsigned Opc;
      const TargetRegisterClass *RC;
      switch (MI.getOpcode()) {
      default:
        ++MII;
        continue;
      case GB::IselLsl8:
        Opc = GB::SLA_r;
        RC = &GB::R8RegClass;
        break;
      case GB::IselLsl16:
        Opc = GB::SHL16;
        RC = &GB::R16RegClass;
        break;
      case GB::IselAsr8:
        Opc = GB::SRA_r;
        RC = &GB::R8RegClass;
        break;
      case GB::IselAsr16:
        Opc = GB::ASR16;
        RC = &GB::R16RegClass;
        break;
      case GB::IselLsr8:
        Opc = GB::SRL_r;
        RC = &GB::R8RegClass;
        break;
      case GB::IselLsr16:
        Opc = GB::LSR16;
        RC = &GB::R16RegClass;
        break;
      case GB::IselRol8:
        Opc = GB::RLC_r;
        RC = &GB::R8RegClass;
        break;
      case GB::IselRol16:
        Opc = GB::ROL16;
        RC = &GB::R16RegClass;
        break;
      case GB::IselRor8:
        Opc = GB::RRC_r;
        RC = &GB::R8RegClass;
        break;
      case GB::IselRor16:
        Opc = GB::ROR16;
        RC = &GB::R16RegClass;
        break;
      }

      MachineFunction::iterator I;
      for (I = MF->begin(); I != MF->end() && I != MBBI; ++I);
      if (I != MF->end()) ++I;

      // Create loop block.
      MachineBasicBlock *LoopBB =
        MF->CreateMachineBasicBlock(MBBI->getBasicBlock());
      MachineBasicBlock *RemBB =
        MF->CreateMachineBasicBlock(MBBI->getBasicBlock());

      MF->insert(I, LoopBB);
      MF->insert(I, RemBB);

      // Update machine-CFG edges by transferring all successors of the current
      // block to the block containing instructions after shift.
      RemBB->splice(RemBB->begin(), &*MBBI, std::next(MachineBasicBlock::iterator(MI)),
                    MBBI->end());
      RemBB->transferSuccessorsAndUpdatePHIs(&*MBBI);

      // Add adges BB => LoopBB => RemBB, BB => RemBB, LoopBB => LoopBB.
      MBBI->addSuccessor(LoopBB);
      MBBI->addSuccessor(RemBB);
      LoopBB->addSuccessor(RemBB);
      LoopBB->addSuccessor(LoopBB);

      unsigned ShiftAmtReg = MRI->createVirtualRegister(&GB::R8RegClass);
      unsigned ShiftAmtReg2 = MRI->createVirtualRegister(&GB::R8RegClass);
      unsigned ShiftReg = MRI->createVirtualRegister(RC);
      unsigned ShiftReg2 = MRI->createVirtualRegister(RC);
      unsigned ShiftAmtSrcReg = MI.getOperand(2).getReg();
      unsigned SrcReg = MI.getOperand(1).getReg();
      unsigned DstReg = MI.getOperand(0).getReg();

      // BB:
      // cpi N, 0
      // breq RemBB
      BuildMI(&*MBBI, dl, TII->get(GB::CP8i))
        .addReg(ShiftAmtSrcReg)
        .addImm(0);
      BuildMI(&*MBBI, dl, TII->get(GB::JP_cc_nn))
        .addMBB(RemBB)
        .addImm(GBCC::COND_Z);

      // LoopBB:
      // ShiftReg = phi [%SrcReg, BB], [%ShiftReg2, LoopBB]
      // ShiftAmt = phi [%N, BB],      [%ShiftAmt2, LoopBB]
      // ShiftReg2 = shift ShiftReg
      // ShiftAmt2 = ShiftAmt - 1;
      BuildMI(LoopBB, dl, TII->get(GB::PHI), ShiftReg)
          .addReg(SrcReg)
          .addMBB(&*MBBI)
          .addReg(ShiftReg2)
          .addMBB(LoopBB);
      BuildMI(LoopBB, dl, TII->get(GB::PHI), ShiftAmtReg)
          .addReg(ShiftAmtSrcReg)
          .addMBB(&*MBBI)
          .addReg(ShiftAmtReg2)
          .addMBB(LoopBB);
      BuildMI(LoopBB, dl, TII->get(Opc), ShiftReg2)
        .addReg(ShiftReg);
      BuildMI(LoopBB, dl, TII->get(GB::DEC_r), ShiftAmtReg2)
          .addReg(ShiftAmtReg);
      BuildMI(LoopBB, dl, TII->get(GB::JP_cc_nn))
        .addMBB(LoopBB)
        .addImm(GBCC::COND_NZ);

      // RemBB:
      // DestReg = phi [%SrcReg, BB], [%ShiftReg, LoopBB]
      BuildMI(*RemBB, RemBB->begin(), dl, TII->get(GB::PHI), DstReg)
          .addReg(SrcReg)
          .addMBB(&*MBBI)
          .addReg(ShiftReg2)
          .addMBB(LoopBB);
      MI.eraseFromParent(); // The pseudo instruction is gone now.
      MII = MBBI->begin();
      MF->getProperties().reset(MachineFunctionProperties::Property::NoPHIs);
      Modified |= true;
    }
  }

  return Modified;
}


bool GBZ80PostISel::optimizeCP() {
  bool Modified = false;


  return Modified;
}

struct Branch16Info {
  MachineInstr *Instr;

  unsigned LHSReg;
  int16_t LHSImm;
  unsigned RHSReg;
  int16_t RHSImm;

  ISD::CondCode CC;

  MachineBasicBlock *True;
  MachineBasicBlock *False;



  Branch16Info(MachineInstr *I, unsigned LHS, unsigned RHS,
               ISD::CondCode CC, MachineBasicBlock *T, MachineBasicBlock *F) {
    this->Instr = I;
    this->CC = CC;
    this->True = T;
    this->False = F;
    initialize(LHS, RHS);
  }

  void initialize(unsigned LHS, unsigned RHS) {
    MachineRegisterInfo &MRI = True->getParent()->getRegInfo();

    LHSReg = LHS;
    RHSReg = RHS;

    // XXX: This can probably be made smarter somehow.
    
    if (!TargetRegisterInfo::isPhysicalRegister(LHS)) {
      MachineInstr *DefI = MRI.getVRegDef(LHS);
      if (DefI->getOpcode() == GB::LD_dd_nn) {
        LHSReg = 0;
        LHSImm = DefI->getOperand(1).getImm();
      }
    }
    if (!TargetRegisterInfo::isPhysicalRegister(RHSReg)) {
      MachineInstr *DefI = MRI.getVRegDef(RHSReg);
      if (DefI->getOpcode() == GB::LD_dd_nn) {
        RHSReg = 0;
        RHSImm = DefI->getOperand(1).getImm();
      }
    }

    canonicalize();
  }

  bool isLHSReg() const { return LHSReg != 0; }
  bool isRHSReg() const { return RHSReg != 0; }

  void canonicalize() {
    // Bring immediates to the right hand side.
    if (!isLHSReg() && isRHSReg()) {
      std::swap(LHSReg, RHSReg);
      std::swap(LHSImm, RHSImm);
      CC = ISD::getSetCCSwappedOperands(CC);
    }

    // TODO: more simplifications? can't really do form optimizations here.
  }
};



bool GBZ80PostISel::expandBranch16() {
  bool Modified = false;

  SmallVector<Branch16Info, 8> Work;
  
  for (auto &MBB : *MF) {
    for (auto MII = MBB.begin(); MII != MBB.end(); ++MII) {
      if (MII->getOpcode() != GB::BR16)
        continue;

      MachineBasicBlock *T = MII->getOperand(0).getMBB();
      MachineInstr *Next = MII->getNextNode();
      MachineBasicBlock *F;
      if (Next == MBB.end())
        F = MBB.getFallThrough();
      else {
        F = Next->getOperand(0).getMBB();
        Next->eraseFromParent();
      }
      assert(F && "No false block?");

      ISD::CondCode CC = (ISD::CondCode)MII->getOperand(1).getImm();
      unsigned LHS = MII->getOperand(2).getReg();
      unsigned RHS = MII->getOperand(3).getReg();

      Work.emplace_back(&*MII, LHS, RHS, CC, T, F);
    }
  }

  /*
    ld a, h
    cp d
    jr nz, .SECOND
    jr .TESTLO
  .TESTLO
    ld a, l
    cp e
  .SECOND
    jr CC, .TRUE
  .FALSE
  */

  for (auto &BI : Work) {
    MachineInstr *MI = BI.Instr;
    DebugLoc dl = MI->getDebugLoc();

    switch (BI.CC) {
    case ISD::SETEQ:
    case ISD::SETNE: {
      /*
        special case for 0, TODO
        cmp BC, 00

      LD A, B
      OR C
      JR Z/NZ, .T
      JR .F

        cmp BC, DE

      loA = COPY LHS:lo
        loR = EXTRACT_SUBREG RHS, lo
      CP loA, loR/loImm
      JP_cc Z/NZ, .F
      LD A, C
      hiA = COPY LHS:hi
        hiR = EXTRACT_SUBREG RHS, lo
      CP hiA, hiR/hiImm
      JP_cc Z/NZ, .F
      JR .T
      */

      unsigned loL = MRI->createVirtualRegister(&GB::R8RegClass);
      BuildMI(*MI->getParent(), MI, dl, TII->get(GB::COPY), loL)
        .addReg(BI.LHSReg, 0, GB::sub_lo);

      if (BI.isRHSReg()) {
        unsigned loR = MRI->createVirtualRegister(&GB::R8RegClass);
        BuildMI(*MI->getParent(), MI, dl, TII->get(GB::COPY), loR)
          .addReg(BI.RHSReg, 0, GB::sub_lo);
        BuildMI(*MI->getParent(), MI, dl, TII->get(GB::CP8r))
          .addReg(loL)
          .addReg(loR);
      } else {
        BuildMI(*MI->getParent(), MI, dl, TII->get(GB::CP8i))
          .addReg(loL)
          .addImm((uint8_t)BI.RHSImm);
      }
      BuildMI(*MI->getParent(), MI, dl, TII->get(GB::JP_cc_nn))
        .addMBB(BI.CC == ISD::SETEQ ? BI.False : BI.True)
        .addImm(GBCC::COND_NZ);
      
      MachineBasicBlock *TopBB = MI->getParent();
      MachineBasicBlock *MidBB = MF->CreateMachineBasicBlock();
      MF->insert(std::next(MI->getParent()->getIterator()), MidBB);

      BuildMI(TopBB, dl, TII->get(GB::JP_nn))
        .addMBB(MidBB);

      MidBB->transferSuccessorsAndUpdatePHIs(TopBB);

      TopBB->addSuccessor(MidBB);
      TopBB->addSuccessor(BI.CC == ISD::SETEQ ? BI.False : BI.True);

      for (MachineBasicBlock *Succ : TopBB->successors()) {
        for (MachineInstr &Phi : *Succ) {
          if (!Phi.isPHI())
            break;
          for (unsigned i = 2; i < Phi.getNumOperands(); i += 2) {
            if (Phi.getOperand(i).getMBB() == MidBB) {
              MachineOperand O = Phi.getOperand(i - 1);
              Phi.addOperand(O);
              Phi.addOperand(MachineOperand::CreateMBB(TopBB));
              break;
            }
          }
        }
      }

      unsigned hiL = MRI->createVirtualRegister(&GB::R8RegClass);
      BuildMI(MidBB, dl, TII->get(GB::COPY), hiL)
        .addReg(BI.LHSReg, 0, GB::sub_hi);

      if (BI.isRHSReg()) {
        unsigned hiR = MRI->createVirtualRegister(&GB::R8RegClass);
        BuildMI(MidBB, dl, TII->get(GB::COPY), hiR)
          .addReg(BI.RHSReg, 0, GB::sub_hi);
        BuildMI(MidBB, dl, TII->get(GB::CP8r))
          .addReg(hiL)
          .addReg(hiR);
      } else {
        BuildMI(MidBB, dl, TII->get(GB::CP8i))
          .addReg(hiL)
          .addImm((uint8_t)(BI.RHSImm >> 8));
      }
      BuildMI(MidBB, dl, TII->get(GB::JP_cc_nn))
        .addMBB(BI.CC == ISD::SETEQ ? BI.False : BI.True)
        .addImm(GBCC::COND_NZ);

      BuildMI(MidBB, dl, TII->get(GB::JP_nn))
        .addMBB(BI.CC == ISD::SETEQ ? BI.True : BI.False);


      //MidBB->addSuccessor(BI.False);
      //MidBB->addSuccessor(BI.True);
      break;
    }
    }

    MI->eraseFromParent();
  }

  return Modified;
}

bool GBZ80PostISel::runOnMachineFunction(MachineFunction &MF) {
  bool Modified = false;

  this->MF = &MF;
  const GBZ80Subtarget &STI = MF.getSubtarget<GBZ80Subtarget>();
  TRI = STI.getRegisterInfo();
  TII = STI.getInstrInfo();
  MRI = &MF.getRegInfo();

  // Expand shift loop pseudos.
  Modified |= expandShiftLoop16();

  Modified |= expandPseudos();

  // Expand branch16 pseudos.
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
