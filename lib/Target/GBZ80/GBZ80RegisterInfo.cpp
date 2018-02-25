//===-- GBZ80RegisterInfo.cpp - GBZ80 Register Information --------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the GBZ80 implementation of the TargetRegisterInfo class.
//
//===----------------------------------------------------------------------===//

#include "GBZ80RegisterInfo.h"

#include "llvm/ADT/BitVector.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/IR/Function.h"
#include "llvm/Target/TargetFrameLowering.h"

#include "GBZ80.h"
#include "GBZ80InstrInfo.h"
#include "GBZ80TargetMachine.h"
#include "MCTargetDesc/GBZ80MCTargetDesc.h"

#define GET_REGINFO_TARGET_DESC
#include "GBZ80GenRegisterInfo.inc"

namespace llvm {

GBZ80RegisterInfo::GBZ80RegisterInfo() : GBZ80GenRegisterInfo(0) {}

const uint16_t *
GBZ80RegisterInfo::getCalleeSavedRegs(const MachineFunction *MF) const {
  return CSR_Normal_SaveList;
#if 0
  CallingConv::ID CC = MF->getFunction()->getCallingConv();

  return ((CC == CallingConv::GBZ80_INTR || CC == CallingConv::GBZ80_SIGNAL)
              ? CSR_Interrupts_SaveList
              : CSR_Normal_SaveList);
#endif
}

const uint32_t *
GBZ80RegisterInfo::getCallPreservedMask(const MachineFunction &MF,
                                      CallingConv::ID CC) const {
  return CSR_Normal_RegMask;
#if 0
  return ((CC == CallingConv::GBZ80_INTR || CC == CallingConv::GBZ80_SIGNAL)
              ? CSR_Interrupts_RegMask
              : CSR_Normal_RegMask);
#endif
}

BitVector GBZ80RegisterInfo::getReservedRegs(const MachineFunction &MF) const {
  BitVector Reserved(getNumRegs());

  // FIXME: Should we reserve A?

  //  Reserve the stack pointer.
  Reserved.set(GB::SP);

  return Reserved;
}

const TargetRegisterClass *
GBZ80RegisterInfo::getLargestLegalSuperClass(const TargetRegisterClass *RC,
                                           const MachineFunction &MF) const {
  const TargetRegisterInfo *TRI = MF.getSubtarget().getRegisterInfo();
  if (RC == &GB::ARegRegClass || RC == &GB::CRegRegClass ||
      RC == &GB::GPR8RegClass) {
    return &GB::GPR8RegClass;
  }

  if (RC == &GB::HLPairsRegClass || RC == &GB::BCDEPairsRegClass ||
      RC == &GB::PairsRegClass) {
    return &GB::PairsRegClass;
  }

  return RC;
}

/// Fold a frame offset shared between two add instructions into a single one.
static void foldFrameOffset(MachineBasicBlock::iterator &II, int &Offset, unsigned DstReg) {
#if 0
  MachineInstr &MI = *II;
  int Opcode = MI.getOpcode();

  // Don't bother trying if the next instruction is not an add or a sub.
  if ((Opcode != GB::SUBIWRdK) && (Opcode != GB::ADIWRdK)) {
    return;
  }

  // Check that DstReg matches with next instruction, otherwise the instruction
  // is not related to stack address manipulation.
  if (DstReg != MI.getOperand(0).getReg()) {
    return;
  }

  // Add the offset in the next instruction to our offset.
  switch (Opcode) {
  case GB::SUBIWRdK:
    Offset += -MI.getOperand(2).getImm();
    break;
  case GB::ADIWRdK:
    Offset += MI.getOperand(2).getImm();
    break;
  }

  // Finally remove the instruction.
  II++;
  MI.eraseFromParent();
#endif
}

void GBZ80RegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator II,
                                          int SPAdj, unsigned FIOperandNum,
                                          RegScavenger *RS) const {
  assert(SPAdj == 0 && "Unexpected SPAdj value");

  MachineInstr &MI = *II;
  DebugLoc dl = MI.getDebugLoc();
  MachineBasicBlock &MBB = *MI.getParent();
  const MachineFunction &MF = *MBB.getParent();
  const GBZ80TargetMachine &TM = (const GBZ80TargetMachine &)MF.getTarget();
  const TargetInstrInfo &TII = *TM.getSubtargetImpl()->getInstrInfo();
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  const TargetFrameLowering *TFI = TM.getSubtargetImpl()->getFrameLowering();
  int FrameIndex = MI.getOperand(FIOperandNum).getIndex();
  int Offset = MFI.getObjectOffset(FrameIndex);

  // Add one to the offset because SP points to an empty slot.
  Offset += MFI.getStackSize() - TFI->getOffsetOfLocalArea() + 1;
  // Fold incoming offset.
  Offset += MI.getOperand(FIOperandNum + 1).getImm();

  // Combine these.
  if (MI.getOpcode() == GB::FRMIDX) {
    // Materialize a frame index.
    // TODO
  }
  else if (MI.getOpcode() == GB::FRMIDX_Load8) {
    // Load an 8 bit value from this frame index.
    // TODO
  }
  else if (MI.getOpcode() == GB::FRMIDX_Store8) {
    // Load an 8 bit value from this frame index.
    // TODO
  }


}

unsigned GBZ80RegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  // The stack pointer is always the frame register.
  return GB::SP;
}

const TargetRegisterClass *
GBZ80RegisterInfo::getPointerRegClass(const MachineFunction &MF,
                                    unsigned Kind) const {
  return &GB::PairsRegClass;
}

const TargetRegisterClass *
GBZ80RegisterInfo::getCrossCopyRegClass(const TargetRegisterClass *RC) const {
  if (RC == &GB::ARegRegClass || RC == &GB::CRegRegClass)
    return &GB::GPR8RegClass;
  else if (RC == &GB::HLPairsRegClass || RC == &GB::BCDEPairsRegClass)
    return &GB::PairsRegClass;
  return RC;
}

void GBZ80RegisterInfo::splitReg(unsigned Reg,
                               unsigned &LoReg,
                               unsigned &HiReg) const {
    assert(GB::PairsRegClass.contains(Reg) && "can only split 16-bit registers");

    LoReg = getSubReg(Reg, GB::sub_lo);
    HiReg = getSubReg(Reg, GB::sub_hi);
}

} // end of namespace llvm
