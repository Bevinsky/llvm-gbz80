//===-- GBZ80InstrInfo.h - GBZ80 Instruction Information ------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the GBZ80 implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_GBZ80_INSTR_INFO_H
#define LLVM_GBZ80_INSTR_INFO_H

#include "llvm/Target/TargetInstrInfo.h"

#include "GBZ80RegisterInfo.h"

#define GET_INSTRINFO_HEADER
#include "GBZ80GenInstrInfo.inc"
#undef GET_INSTRINFO_HEADER

namespace llvm {

namespace GBCC {

/// GBZ80 specific condition codes.
/// These correspond to `GBZ80_*_COND` in `GBZ80InstrInfo.td`.
/// They must be kept in synch.
  enum CondCodes {
  COND_INVALID = 0,
  COND_NZ, // i8 _NE
  COND_Z,  // i8 _EQ
  COND_NC, // i8 _UGE
  COND_C,  // i8 _ULT
};

} // end of namespace GBCC

namespace GBII {

/// Specifies a target operand flag.
enum TOF {
  MO_NO_FLAG,

  /// On a symbol operand, this represents the lo part.
  MO_LO = (1 << 1),

  /// On a symbol operand, this represents the hi part.
  MO_HI = (1 << 2),

  /// On a symbol operand, this represents it has to be negated.
  MO_NEG = (1 << 3)
};

} // end of namespace GBII

typedef std::tuple<GBCC::CondCodes, bool, GBCC::CondCodes, bool> CCPair;

/// Utilities related to the GBZ80 instruction set.
class GBZ80InstrInfo : public GBZ80GenInstrInfo {
public:
  
  static std::map<CCPair, unsigned> CCPair2CPClass;
  static std::map<unsigned, std::vector<CCPair>> CPClass2CCPairs;

  explicit GBZ80InstrInfo();

  const GBZ80RegisterInfo &getRegisterInfo() const { return RI; }
  GBCC::CondCodes getOppositeCondition(GBCC::CondCodes CC) const;

  bool isConditionalBranch(const MachineInstr &) const;
  bool isUnconditionalBranch(const MachineInstr &) const;
  // Invert the condition code given by Cond. Returns true if it was
  // possible.
  bool invertCondition(SmallVectorImpl<MachineOperand> &Cond) const;
  // Alter the condition to prepare for swapping the operands to a CP.
  bool swapCondition(SmallVectorImpl<MachineOperand> &Cond) const;
  // Swap the less-than and greater-than parts of the CP equivalence class.
  unsigned swapCPClass(unsigned Class) const {
    return ((Class & 0x1) << 2) | ((Class & 0x4) >> 2) | (Class & 0x2);
  }

  unsigned getInstSizeInBytes(const MachineInstr &MI) const override;

  void copyPhysReg(MachineBasicBlock &MBB, MachineBasicBlock::iterator MI,
                   const DebugLoc &DL, unsigned DestReg, unsigned SrcReg,
                   bool KillSrc) const override;
  void storeRegToStackSlot(MachineBasicBlock &MBB,
                           MachineBasicBlock::iterator MI, unsigned SrcReg,
                           bool isKill, int FrameIndex,
                           const TargetRegisterClass *RC,
                           const TargetRegisterInfo *TRI) const override;
  void loadRegFromStackSlot(MachineBasicBlock &MBB,
                            MachineBasicBlock::iterator MI, unsigned DestReg,
                            int FrameIndex, const TargetRegisterClass *RC,
                            const TargetRegisterInfo *TRI) const override;
  unsigned isLoadFromStackSlot(const MachineInstr &MI,
                               int &FrameIndex) const override;
  unsigned isStoreToStackSlot(const MachineInstr &MI,
                              int &FrameIndex) const override;

  bool isReallyTriviallyReMaterializable(const MachineInstr &MI,
    AliasAnalysis *AA) const override;
  void reMaterialize(MachineBasicBlock &MBB,
    MachineBasicBlock::iterator MI, unsigned DestReg,
    unsigned SubIdx, const MachineInstr &Orig,
    const TargetRegisterInfo &TRI) const override;

  MachineInstr *commuteInstructionImpl(MachineInstr &MI, bool NewMI,
    unsigned OpIdx1,
    unsigned OpIdx2) const override;

  // Branch analysis.
  bool analyzeBranch(MachineBasicBlock &MBB, MachineBasicBlock *&TBB,
                     MachineBasicBlock *&FBB,
                     SmallVectorImpl<MachineOperand> &Cond,
                     bool AllowModify = false) const override;
  unsigned insertBranch(MachineBasicBlock &MBB, MachineBasicBlock *TBB,
                        MachineBasicBlock *FBB, ArrayRef<MachineOperand> Cond,
                        const DebugLoc &DL,
                        int *BytesAdded = nullptr) const override;
  unsigned removeBranch(MachineBasicBlock &MBB,
                        int *BytesRemoved = nullptr) const override;
  bool
  reverseBranchCondition(SmallVectorImpl<MachineOperand> &Cond) const override;
  virtual bool analyzeBranchPredicate(MachineBasicBlock &MBB,
    MachineBranchPredicate &MBP,
    bool AllowModify = false) const;

  MachineBasicBlock *getBranchDestBlock(const MachineInstr &MI) const override;

  bool isBranchOffsetInRange(unsigned BranchOpc,
                             int64_t BrOffset) const override;

  int getSPAdjust(const MachineInstr &MI) const override;

  unsigned insertIndirectBranch(MachineBasicBlock &MBB,
                                MachineBasicBlock &NewDestBB,
                                const DebugLoc &DL,
                                int64_t BrOffset,
                                RegScavenger *RS) const override;
private:
  const GBZ80RegisterInfo RI;
};

} // end namespace llvm

#endif // LLVM_GBZ80_INSTR_INFO_H
