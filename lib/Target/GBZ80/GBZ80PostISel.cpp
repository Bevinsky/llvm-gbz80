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
  MachineInstr *expandSelect(MachineInstr &);
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
      JR_cc Z/NZ, .F
      LD A, C
      hiA = COPY LHS:hi
        hiR = EXTRACT_SUBREG RHS, lo
      CP hiA, hiR/hiImm
      JR_cc Z/NZ, .F
      JR .T
      */

      unsigned loA = MRI->createVirtualRegister(&GB::ARegRegClass);
      BuildMI(*MI->getParent(), MI, dl, TII->get(GB::COPY), loA)
        .addReg(BI.LHSReg, 0, GB::sub_lo);

      if (BI.isRHSReg()) {
        unsigned loR = MRI->createVirtualRegister(&GB::GPR8RegClass);
        BuildMI(*MI->getParent(), MI, dl, TII->get(GB::COPY), loR)
          .addReg(BI.RHSReg, 0, GB::sub_lo);
        BuildMI(*MI->getParent(), MI, dl, TII->get(GB::CP_r))
          .addReg(loA)
          .addReg(loR);
      } else {
        BuildMI(*MI->getParent(), MI, dl, TII->get(GB::CP_n))
          .addReg(loA)
          .addImm((uint8_t)BI.RHSImm);
      }
      BuildMI(*MI->getParent(), MI, dl, TII->get(GB::JR_cc_e))
        .addMBB(BI.CC == ISD::SETEQ ? BI.False : BI.True)
        .addImm(GBCC::COND_NZ);
      
      MachineBasicBlock *TopBB = MI->getParent();
      MachineBasicBlock *MidBB = MF->CreateMachineBasicBlock();
      MF->insert(std::next(MI->getParent()->getIterator()), MidBB);

      BuildMI(TopBB, dl, TII->get(GB::JR_e))
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

      unsigned hiA = MRI->createVirtualRegister(&GB::ARegRegClass);
      BuildMI(MidBB, dl, TII->get(GB::COPY), hiA)
        .addReg(BI.LHSReg, 0, GB::sub_hi);

      if (BI.isRHSReg()) {
        unsigned hiR = MRI->createVirtualRegister(&GB::GPR8RegClass);
        BuildMI(MidBB, dl, TII->get(GB::COPY), hiR)
          .addReg(BI.RHSReg, 0, GB::sub_hi);
        BuildMI(MidBB, dl, TII->get(GB::CP_r))
          .addReg(hiA)
          .addReg(hiR);
      } else {
        BuildMI(MidBB, dl, TII->get(GB::CP_n))
          .addReg(hiA)
          .addImm((uint8_t)(BI.RHSImm >> 8));
      }
      BuildMI(MidBB, dl, TII->get(GB::JR_cc_e))
        .addMBB(BI.CC == ISD::SETEQ ? BI.False : BI.True)
        .addImm(GBCC::COND_NZ);

      BuildMI(MidBB, dl, TII->get(GB::JR_e))
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
