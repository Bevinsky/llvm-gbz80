//===-- GBZ80RegisterInfo.h - GBZ80 Register Information Impl -------*- C++ -*-===//
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

#ifndef LLVM_GBZ80_REGISTER_INFO_H
#define LLVM_GBZ80_REGISTER_INFO_H

#include "llvm/CodeGen/TargetRegisterInfo.h"

#define GET_REGINFO_HEADER
#include "GBZ80GenRegisterInfo.inc"

namespace llvm {

/// Utilities relating to GBZ80 registers.
class GBZ80RegisterInfo : public GBZ80GenRegisterInfo {
public:
  GBZ80RegisterInfo();

public:
  const uint16_t *
  getCalleeSavedRegs(const MachineFunction *MF = 0) const override;
  const uint32_t *getCallPreservedMask(const MachineFunction &MF,
                                       CallingConv::ID CC) const override;
  BitVector getReservedRegs(const MachineFunction &MF) const override;

  const TargetRegisterClass *
  getLargestLegalSuperClass(const TargetRegisterClass *RC,
                            const MachineFunction &MF) const override;

  /// Stack Frame Processing Methods
  void eliminateFrameIndex(MachineBasicBlock::iterator MI, int SPAdj,
                           unsigned FIOperandNum,
                           RegScavenger *RS = NULL) const override;

  unsigned getFrameRegister(const MachineFunction &MF) const override;

  const TargetRegisterClass *
  getPointerRegClass(const MachineFunction &MF,
                     unsigned Kind = 0) const override;
  const TargetRegisterClass *
    getCrossCopyRegClass(const TargetRegisterClass *RC) const;

  bool shouldCoalesce(MachineInstr *MI,
    const TargetRegisterClass *SrcRC,
    unsigned SubReg,
    const TargetRegisterClass *DstRC,
    unsigned DstSubReg,
    const TargetRegisterClass *NewRC,
    LiveIntervals &LIS) const override;

  bool trackLivenessAfterRegAlloc(const MachineFunction &MF) const override {
    return true;
  }

  /// Splits a 16-bit `DREGS` register into the lo/hi register pair.
  /// \param Reg A 16-bit register to split.
  void splitReg(unsigned Reg, unsigned &LoReg, unsigned &HiReg) const;
};

} // end namespace llvm

#endif // LLVM_GBZ80_REGISTER_INFO_H
